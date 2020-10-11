package codes.quine.labo.redos
package regexp

import scala.collection.mutable

import com.ibm.icu.lang.UCharacter

import Pattern._

/** Pattern is ECMA-262 RegExp pattern data.
  *
  * @param flagSet a flag set of this pattern
  * @param root a root node of this pattern
  */
final case class Pattern(flagSet: FlagSet, root: Node) {
  override def toString: String =
    s"/${nodeToString(root)}/${flagSetToString(flagSet)}"
}

object Pattern {
  final case class FlagSet(
      global: Boolean,
      ignoreCase: Boolean,
      multiline: Boolean,
      dotAll: Boolean,
      unicode: Boolean,
      sticky: Boolean
  )

  sealed trait Node
  sealed trait ClassItem

  final case class Disjunction(children: Seq[Node]) extends Node
  final case class Sequence(children: Seq[Node]) extends Node
  final case class Capture(child: Node) extends Node
  final case class NamedCapture(name: String, child: Node) extends Node
  final case class Group(child: Node) extends Node
  final case class Star(nonGreedy: Boolean, child: Node) extends Node
  final case class Plus(nonGreedy: Boolean, child: Node) extends Node
  final case class Question(nonGreedy: Boolean, child: Node) extends Node
  final case class Repeat(nonGreedy: Boolean, min: Int, max: Option[Option[Int]], child: Node) extends Node
  final case class WordBoundary(invert: Boolean) extends Node
  case object LineBegin extends Node
  case object LineEnd extends Node
  final case class LookAhead(negative: Boolean, child: Node) extends Node
  final case class LookBehind(negative: Boolean, child: Node) extends Node
  final case class Character(value: Int) extends Node with ClassItem
  final case class SimpleEscapeClass(invert: Boolean, kind: EscapeClassKind) extends Node with ClassItem
  final case class UnicodeProperty(invert: Boolean, property: String) extends Node with ClassItem
  final case class UnicodePropertyValue(invert: Boolean, property: String, value: String) extends Node with ClassItem
  final case class CharacterClass(invert: Boolean, children: Seq[ClassItem]) extends Node
  final case class ClassRange(begin: Int, end: Int) extends ClassItem
  case object Dot extends Node
  final case class BackReference(index: Int) extends Node
  final case class NamedBackReference(name: String) extends Node

  sealed trait EscapeClassKind
  object EscapeClassKind {
    case object Digit extends EscapeClassKind
    case object Word extends EscapeClassKind
    case object Space extends EscapeClassKind
  }

  private def nodeToString(node: Node): String =
    node match {
      case Disjunction(ns)                                 => ns.map(nodeToStringInDisjunction(_)).mkString("|")
      case Sequence(ns)                                    => ns.map(nodeToStringInSequence(_)).mkString
      case Capture(n)                                      => s"(${nodeToString(n)})"
      case NamedCapture(name, n)                           => s"(?<$name>${nodeToString(n)})"
      case Group(n)                                        => s"(?:${nodeToString(n)})"
      case Star(false, n)                                  => s"${nodeToStringInRepeat(n)}*"
      case Star(true, n)                                   => s"${nodeToStringInRepeat(n)}*?"
      case Plus(false, n)                                  => s"${nodeToStringInRepeat(n)}+"
      case Plus(true, n)                                   => s"${nodeToStringInRepeat(n)}+?"
      case Question(false, n)                              => s"${nodeToStringInRepeat(n)}?"
      case Question(true, n)                               => s"${nodeToStringInRepeat(n)}??"
      case Repeat(false, min, None, n)                     => s"${nodeToStringInRepeat(n)}{$min}"
      case Repeat(true, min, None, n)                      => s"${nodeToStringInRepeat(n)}{$min}?"
      case Repeat(false, min, Some(None), n)               => s"${nodeToStringInRepeat(n)}{$min,}"
      case Repeat(true, min, Some(None), n)                => s"${nodeToStringInRepeat(n)}{$min,}?"
      case Repeat(false, min, Some(Some(max)), n)          => s"${nodeToStringInRepeat(n)}{$min,$max}"
      case Repeat(true, min, Some(Some(max)), n)           => s"${nodeToStringInRepeat(n)}{$min,$max}?"
      case WordBoundary(false)                             => "\\b"
      case WordBoundary(true)                              => "\\B"
      case LineBegin                                       => "^"
      case LineEnd                                         => "$"
      case LookAhead(false, n)                             => s"(?=${nodeToString(n)})"
      case LookAhead(true, n)                              => s"(?!${nodeToString(n)})"
      case LookBehind(false, n)                            => s"(?<=${nodeToString(n)})"
      case LookBehind(true, n)                             => s"(?<!${nodeToString(n)})"
      case Character(c)                                    => characterToString(c)
      case CharacterClass(false, items)                    => s"[${items.map(classItemToString(_)).mkString}]"
      case CharacterClass(true, items)                     => s"[^${items.map(classItemToString(_)).mkString}]"
      case SimpleEscapeClass(false, EscapeClassKind.Digit) => "\\d"
      case SimpleEscapeClass(true, EscapeClassKind.Digit)  => "\\D"
      case SimpleEscapeClass(false, EscapeClassKind.Word)  => "\\w"
      case SimpleEscapeClass(true, EscapeClassKind.Word)   => "\\W"
      case SimpleEscapeClass(false, EscapeClassKind.Space) => "\\s"
      case SimpleEscapeClass(true, EscapeClassKind.Space)  => "\\S"
      case UnicodeProperty(false, p)                       => s"\\p{$p}"
      case UnicodeProperty(true, p)                        => s"\\P{$p}"
      case UnicodePropertyValue(false, p, v)               => s"\\p{$p=$v}"
      case UnicodePropertyValue(true, p, v)                => s"\\P{$p=$v}"
      case Dot                                             => "."
      case BackReference(i)                                => s"\\$i"
      case NamedBackReference(name)                        => s"\\k<$name>"
    }

  private def nodeToStringInDisjunction(node: Node): String =
    node match {
      case _: Disjunction => s"(?:${nodeToString(node)})"
      case _              => nodeToString(node)
    }

  private def nodeToStringInSequence(node: Node): String =
    node match {
      case _: Disjunction | _: Sequence => s"(?:${nodeToString(node)})"
      case _                            => nodeToString(node)
    }

  def nodeToStringInRepeat(node: Node): String =
    node match {
      case _: Disjunction | _: Sequence | _: Star | _: Plus | _: Question | _: Repeat | _: WordBoundary | LineBegin |
          LineEnd | _: LookAhead | _: LookBehind =>
        s"(?:${nodeToString(node)})"
      case _ => nodeToString(node)
    }

  private def classItemToString(item: ClassItem): String =
    item match {
      case Character(c)       => characterToStringInClass(c)
      case ClassRange(c1, c2) => s"${characterToStringInClass(c1)}-${characterToStringInClass(c2)}"
      case node: Node         => nodeToString(node)
    }

  private def flagSetToString(flagSet: FlagSet): String = {
    val sb = new mutable.StringBuilder
    if (flagSet.global) sb.append('g')
    if (flagSet.ignoreCase) sb.append('i')
    if (flagSet.multiline) sb.append('m')
    if (flagSet.dotAll) sb.append('s')
    if (flagSet.unicode) sb.append('u')
    if (flagSet.sticky) sb.append('y')
    sb.result()
  }

  private[regexp] val SYNTAX_CHAR_SET = "^$\\.*+?()[]{}|/".toCharArray().toSet

  private def characterToString(c: Int): String =
    c match {
      case 0x09                                                     => "\\t"
      case 0x0a                                                     => "\\n"
      case 0x0b                                                     => "\\v"
      case 0x0c                                                     => "\\f"
      case 0x0d                                                     => "\\r"
      case c if c.isValidChar && SYNTAX_CHAR_SET.contains(c.toChar) => s"\\${c.toChar}"
      case c if 1 <= c && c < 32                                    => s"\\c${(c + 0x40).toChar}"
      case c if UCharacter.isPrintable(c)                           => String.valueOf(UCharacter.toChars(c))
      case c if c < 0x100                                           => f"\\x$c%02x"
      case c if c < 0x10000                                         => f"\\u$c%04x"
      case c                                                        => f"\\u{$c%x}"
    }

  private def characterToStringInClass(c: Int): String =
    c match {
      case 0x09                           => "\\t"
      case 0x0a                           => "\\n"
      case 0x0b                           => "\\v"
      case 0x0c                           => "\\f"
      case 0x0d                           => "\\r"
      case 0x2d                           => "\\-"
      case 0x5b                           => s"\\["
      case 0x5d                           => s"\\]"
      case 0x5e                           => "\\^"
      case c if 1 <= c && c < 32          => s"\\c${(c + 0x40).toChar}"
      case c if UCharacter.isPrintable(c) => String.valueOf(UCharacter.toChars(c))
      case c if c < 0x100                 => f"\\x$c%02x"
      case c if c < 0x10000               => f"\\u$c%04x"
      case c                              => f"\\u{$c%x}"
    }
}
