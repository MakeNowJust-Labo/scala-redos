package codes.quine.labo.redos
package regexp

import scala.annotation.switch
import scala.util.{Try, Success, Failure}

import com.ibm.icu.lang.{UCharacter, UProperty}
import com.ibm.icu.text.UnicodeSet

import fastparse._, NoWhitespace._

import Pattern.{Node, ClassItem, FlagSet, Character}
import Parser.{ID_START, ID_CONTINUE}

object Parser {

  /**
    * Parse ECMA-262 RegExp string.
    *
    * @param source a source string
    * @param flags a flag set string
    * @param additional whether additional features are enabled or not
    * @return Success with parsed RegExp pattern if parsing is succeded, or Failure
    */
  def parse(source: String, flags: String, additional: Boolean = true): Try[Pattern] =
    for {
      flagSet <- parseFlagSet(flags)
      (hasNamedCapture, captures) = preprocessParens(source)
      result = fastparse.parse(source, new Parser(flagSet.unicode, additional, hasNamedCapture, captures).Source(_))
      root <- result match {
        case Parsed.Success(root, _) => Success(root)
        case fail: Parsed.Failure    => Failure(new InvalidRegExpException(s"Parsing failure at ${fail.index}"))
      }
    } yield Pattern(flagSet, root)

  private val ID_START = new UnicodeSet()
    .applyIntPropertyValue(UProperty.ID_START, 1)
    .freeze()

  private val ID_CONTINUE = new UnicodeSet()
    .applyIntPropertyValue(UProperty.ID_CONTINUE, 1)
    .freeze()

  /**
    * Parse a flag set string.
    *
    * @param s a flag set string
    * @return Success with parsed flag set if parsing is succeeded, or Failure
    */
  private def parseFlagSet(s: String): Try[FlagSet] = {
    val cs = s.toList
    // A flag set accept neither duplicated character nor unknown character.
    if (cs.distinct != cs) Failure(new InvalidRegExpException("Duplicated flag"))
    else if (!cs.forall("gimsuy".contains(_))) Failure(new InvalidRegExpException("Unknown flag"))
    else
      Success(
        FlagSet(
          cs.contains('g'),
          cs.contains('i'),
          cs.contains('m'),
          cs.contains('s'),
          cs.contains('u'),
          cs.contains('y')
        )
      )
  }

  /**
    * Count capture parentheses in the source and determine the source contains named capture.
    *
    * @param s a source string
    * @return the first value is a flag whether the source contains named capture or not,
    *   and the second value is capture parentheses number in the source.
    */
  private def preprocessParens(s: String): (Boolean, Int) = {
    var i = 0
    var hasNamedCapture = false
    var captures = 0
    while (i < s.length) {
      (s.charAt(i): @switch) match {
        case '(' =>
          if (s.startsWith("(?", i)) {
            // A named capture is started with "(?<",
            // but it should not start with "(?<=" or "(?<!" dut to look-behind assertion.
            if (s.startsWith("(?<", i) && !s.startsWith("(?<=", i) && !s.startsWith("(?<!", i)) {
              hasNamedCapture = true
              captures += 1
            }
          } else {
            captures += 1
          }
          i += 1
        // Skip character class, escaped character and ordinal character.
        case '[' =>
          i += 1
          while (i < s.length && s.charAt(i) != ']') {
            (s.charAt(i): @switch) match {
              case '\\' => i += 2
              case _    => i += 1
            }
          }
        case '\\' => i += 2
        case _    => i += 1
      }
    }
    (hasNamedCapture, captures)
  }
}

/**
  * Parser is ECMA-262 RegExp parser implementation.
  *
  * ECMA-262 RegExp syntax is modified when unicode flag is enabled or/and source has named captures.
  * So, its constuctor takes these parameters.
  *
  * @param unicode whether unicode flag is enabled or not
  * @param additional whether additional features are enabled or not
  * @param hasNamedCapture whether the soutce has named captures
  * @param captures capture parentheses number
  */
final private class Parser(
    val unicode: Boolean,
    val additional: Boolean,
    val hasNamedCapture: Boolean,
    val captures: Int
) {
  def Source[_: P]: P[Node] = P(Disjunction ~ End)

  private def Disjunction[_: P]: P[Node] =
    P {
      (Sequence ~ ("|" ~ Sequence).rep).map {
        case (node, Seq()) => node
        case (node, nodes) => Pattern.Disjunction(node +: nodes)
      }
    }

  private def Sequence[_: P]: P[Node] =
    P {
      (!CharPred(isSequenceDelimiter(_)) ~ Quantifier).rep.map {
        case Seq(node) => node
        case nodes     => Pattern.Sequence(nodes)
      }
    }

  private def Quantifier[_: P]: P[Node] =
    P {
      Atom.flatMap {
        case node: Pattern.LookAhead if !additional => Pass(node)
        case node: Pattern.LookBehind               => Pass(node)
        case node: Pattern.WordBoundary             => Pass(node)
        case Pattern.LineBegin                      => Pass(Pattern.LineBegin)
        case Pattern.LineEnd                        => Pass(Pattern.LineEnd)
        case node =>
          Repeat.map { case (nonGreedy, min, max) =>
            Pattern.Repeat(nonGreedy, min, max, node)
          } |
            ("*?" ~ Pass(Pattern.Star(true, node))) |
            ("*" ~ Pass(Pattern.Star(false, node))) |
            ("+?" ~ Pass(Pattern.Plus(true, node))) |
            ("+" ~ Pass(Pattern.Plus(false, node))) |
            ("??" ~ Pass(Pattern.Question(true, node))) |
            ("?" ~ Pass(Pattern.Question(false, node))) |
            Pass(node)
      }
    }

  private def Repeat[_: P]: P[(Boolean, Int, Option[Option[Int]])] =
    P {
      (
        (if (additional && !unicode) ("{": P[Unit]) else "{"./) ~
          Digits ~
          ("," ~ (Digits.map(n => Option(Option(n))) | Pass(Option(None))) | Pass(None)) ~ "}" ~
          (("?": P[Unit]).map(_ => true) | Pass(false))
      ).map { case (min, max, nonGreedy) =>
        (nonGreedy, min, max)
      }
    }

  private def Atom[_: P]: P[Node] =
    P {
      (".": P[Unit]).map(_ => Pattern.Dot) |
        ("^": P[Unit]).map(_ => Pattern.LineBegin) |
        ("$": P[Unit]).map(_ => Pattern.LineEnd) |
        Class | Escape | Paren |
        (CharIn("*+?)|") ~/ Fail) |
        (
          if (additional && !unicode) &(Repeat) ~/ Fail
          else CharIn("{}]") ~/ Fail
        ) |
        Character.map(Pattern.Character(_))
    }

  private def Class[_: P]: P[Node] =
    P {
      ("[" ~/ (("^": P[Unit]).map(_ => true) | Pass(false)) ~ (!"]" ~/ ClassItem).rep ~ "]").map {
        case (invert, items) => Pattern.CharacterClass(invert, items)
      }
    }

  private def ClassItem[_: P]: P[ClassItem] =
    P {
      (ClassAtom ~ ((&("-") ~ !"-]" ~ Pass(true)) | Pass(false)))./.flatMap {
        case (Left(c), false)                              => Pass(Pattern.Character(c))
        case (Right(node), false)                          => Pass(node)
        case (Right(node), true) if additional && !unicode => Pass(node)
        case (Right(node), true)                           => Fail
        case (Left(c), true) if additional && !unicode =>
          &("-" ~ EscapeClass) ~ Pass(Pattern.Character(c)) |
            ("-" ~ ClassCharacter).map(Pattern.ClassRange(c, _))
        case (Left(c), true) =>
          ("-" ~ ClassCharacter).map(Pattern.ClassRange(c, _))
      }
    }

  private def ClassAtom[_: P]: P[Either[Int, ClassItem]] =
    P {
      EscapeClass.map(Right(_)) | ClassCharacter.map(Left(_))
    }

  private def ClassCharacter[_: P]: P[Int] =
    P {
      ("\\-" ~ Pass(0x2d)) | ("\\b" ~ Pass(0x08)) | Character | EscapeCharacter
    }

  private def Escape[_: P]: P[Node] =
    P {
      WordBoundary |
        (if (hasNamedCapture) NamedBackReference else Fail) |
        BackReference |
        EscapeClass |
        EscapeCharacter.map(Pattern.Character(_))
    }

  private def WordBoundary[_: P]: P[Node] =
    P {
      "\\b" ~ Pass(Pattern.WordBoundary(false)) |
        "\\B" ~ Pass(Pattern.WordBoundary(true))
    }

  private def BackReference[_: P]: P[Node] =
    P {
      "\\" ~ !"0" ~ Digits.flatMap {
        case x if additional && !unicode =>
          if (x <= captures) Pass(Pattern.BackReference(x)) else Fail
        case x => Pass(Pattern.BackReference(x))
      }
    }

  private def NamedBackReference[_: P]: P[Node] =
    P {
      ("\\k<" ~ CaptureName ~ ">").map(Pattern.NamedBackReference(_))
    }

  private def EscapeClass[_: P]: P[Node with ClassItem] =
    P {
      ("\\w" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Word))) |
        ("\\W" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Word))) |
        ("\\d" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Digit))) |
        ("\\D" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Digit))) |
        ("\\s" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Space))) |
        ("\\S" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Space))) |
        (if (unicode) UnicodeEscapeClass else Fail)
    }

  private def UnicodeEscapeClass[_: P]: P[Node with ClassItem] =
    P {
      (("\\p{" ~ Pass(false) | "\\P{" ~ Pass(true)) ~/ UnicodePropertyName ~ ("=" ~ UnicodePropertyValue).? ~/ "}")
        .map {
          case (invert, p, None)    => Pattern.UnicodeProperty(invert, p)
          case (invert, p, Some(v)) => Pattern.UnicodePropertyValue(invert, p, v)
        }
    }

  private def UnicodePropertyName[_: P]: P[String] = P(CharsWhile(isUnicodeProperty(_)).!)

  private def UnicodePropertyValue[_: P]: P[String] = P(CharsWhile(isUnicodePropertyValue(_)).!)

  private def EscapeCharacter[_: P]: P[Int] =
    P {
      UnicodeEscape |
        ("\\t" ~ Pass(0x09)) |
        ("\\n" ~ Pass(0x0a)) |
        ("\\v" ~ Pass(0x0b)) |
        ("\\f" ~ Pass(0x0c)) |
        ("\\r" ~ Pass(0x0d)) |
        ("\\c" ~ CharPred(isControl(_)).!.map(_.charAt(0) % 32)) |
        ("\\x" ~ HexDigit.rep(exactly = 2).!.map(Integer.parseInt(_, 16))) |
        ("\\0" ~ !CharPred(isDigit(_)) ~ Pass(0)) |
        OctalEscape |
        IdentityEscape
    }

  private def UnicodeEscape[_: P]: P[Int] =
    if (!unicode) P("\\u" ~/ HexDigit.rep(exactly = 4).!.map(Integer.parseInt(_, 16)))
    else
      P {
        "\\u{" ~/ HexDigit.rep(1).!.map(Integer.parseInt(_, 16)).filter(isValidCodePoint(_)) ~/ "}" |
          "\\u" ~/ HexDigit.rep(exactly = 4).!.map(Integer.parseInt(_, 16)).flatMap {
            case x if 0xd800 <= x && x <= 0xdbff =>
              ("\\u" ~ ("D" ~ CharIn("cdefCDEF") ~ HexDigit.rep(exactly = 2)).!.map { s =>
                val y = Integer.parseInt(s, 16)
                0x10000 + (x - 0xd800) * 0x400 + (y - 0xdc00)
              }) | Pass(x)
            case x => Pass(x)
          }
      }

  private def OctalEscape[_: P]: P[Int] =
    if (additional && !unicode)
      P {
        "\\" ~ (
          CharIn("0123") ~ CharIn("01234567").rep(max = 2) |
            CharIn("4567") ~ CharIn("01234567").?
        ).!.map(Integer.parseInt(_, 8))
      }
    else Fail

  private def IdentityEscape[_: P]: P[Int] =
    if (unicode) P("\\" ~ CharPred(isSyntax(_)).!.map(_.charAt(0).toInt))
    else if (additional)
      P {
        "\\" ~ &("c") ~ Pass(0x5c) |
          "\\" ~ (if (hasNamedCapture) CharPred(_ == 'k') else AnyChar).!.map(_.charAt(0).toInt)
      }
    else "\\" ~ CharPred(!ID_CONTINUE.contains(_)).!.map(_.charAt(0).toInt)

  private def Character[_: P]: P[Int] =
    if (unicode)
      P {
        !"\\" ~ (CharPred(_.isHighSurrogate) ~ CharPred(_.isLowSurrogate)).!.map(_.codePointAt(0)) |
          !"\\" ~ AnyChar.!.map(_.charAt(0).toInt)
      }
    else P(!"\\" ~ AnyChar.!.map(_.charAt(0).toInt))

  private def Paren[_: P]: P[Node] =
    P {
      ("(" ~ !"?" ~/ Disjunction ~ ")").map(Pattern.Capture(_)) |
        ("(?:" ~/ Disjunction ~ ")").map(Pattern.Group(_)) |
        ("(?=" ~/ Disjunction ~ ")").map(Pattern.LookAhead(false, _)) |
        ("(?!" ~/ Disjunction ~ ")").map(Pattern.LookAhead(true, _)) |
        ("(?<=" ~/ Disjunction ~ ")").map(Pattern.LookBehind(false, _)) |
        ("(?<!" ~/ Disjunction ~ ")").map(Pattern.LookBehind(true, _)) |
        ("(?<" ~/ CaptureName ~ ">" ~/ Disjunction ~ ")").map { case (name, node) =>
          Pattern.NamedCapture(name, node)
        }
    }

  private def CaptureName[_: P]: P[String] =
    P {
      (CaptureNameChar.filter(isIDStart(_)) ~ CaptureNameChar.filter(isIDPart(_)).rep).map { case (x, xs) =>
        String.valueOf((x +: xs).toArray.flatMap(UCharacter.toChars(_)))
      }
    }

  private def CaptureNameChar[_: P]: P[Int] = P(UnicodeEscape | Character)

  private def Digits[_: P]: P[Int] = P(CharsWhile(isDigit(_)).!.map(_.toInt))

  private def HexDigit[_: P]: P[Unit] = P(CharPred(isHexDigit(_)))

  private def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9'

  private def isHexDigit(c: Char): Boolean =
    isDigit(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'

  private def isControl(c: Char): Boolean =
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

  private def isSyntax(c: Char): Boolean =
    Pattern.SYNTAX_CHAR_SET.contains(c)

  private def isSequenceDelimiter(c: Char): Boolean =
    c == '|' || c == ')'

  private def isUnicodeProperty(c: Char): Boolean =
    isControl(c) || c == '_'

  private def isUnicodePropertyValue(c: Char): Boolean =
    isUnicodeProperty(c) || isDigit(c)

  private def isIDStart(c: Int): Boolean =
    c == '$' || c == '_' || ID_START.contains(c)

  private def isIDPart(c: Int): Boolean =
    c == '$' || c == 0x200c || c == 0x200d || ID_CONTINUE.contains(c)

  private def isValidCodePoint(c: Int): Boolean =
    0 <= c && c < 0x110000
}
