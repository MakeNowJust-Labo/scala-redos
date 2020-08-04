package codes.quine.labo.redos
package regexp

import minitest.SimpleTestSuite

import Pattern._

object ParserSuite extends SimpleTestSuite {
  test("Parser.parse") {
    val testCases = Seq(
      "a" -> Character('a'.toInt),
      "\\012" -> Character(10),
      "\\51" -> Character(41),
      "\\n" -> Character(10),
      "\\x0A" -> Character(10),
      "\\u000A" -> Character(10),
      "[a]" -> CharacterClass(false, Seq(Character('a'.toInt))),
      "[^a]" -> CharacterClass(true, Seq(Character('a'.toInt))),
      "[a-b]" -> CharacterClass(false, Seq(ClassRange('a'.toInt, 'b'.toInt))),
      "[\\]]" -> CharacterClass(false, Seq(Character(']'.toInt))),
      "\\w" -> SimpleEscapeClass(false, EscapeClassKind.Word),
      "\\W" -> SimpleEscapeClass(true, EscapeClassKind.Word),
      "\\d" -> SimpleEscapeClass(false, EscapeClassKind.Digit),
      "\\D" -> SimpleEscapeClass(true, EscapeClassKind.Digit),
      "\\s" -> SimpleEscapeClass(false, EscapeClassKind.Space),
      "\\S" -> SimpleEscapeClass(true, EscapeClassKind.Space),
      "." -> Dot,
      ".." -> Sequence(Seq(Dot, Dot)),
      ".|." -> Disjunction(Seq(Dot, Dot)),
      ".*" -> Star(false, Dot),
      ".*?" -> Star(true, Dot),
      ".+" -> Plus(false, Dot),
      ".+?" -> Plus(true, Dot),
      ".?" -> Question(false, Dot),
      ".??" -> Question(true, Dot),
      ".{2}" -> Repeat(false, 2, None, Dot),
      ".{2,}" -> Repeat(false, 2, Some(None), Dot),
      ".{2,3}" -> Repeat(false, 2, Some(Some(3)), Dot),
      ".{2}?" -> Repeat(true, 2, None, Dot),
      ".{2,}?" -> Repeat(true, 2, Some(None), Dot),
      ".{2,3}?" -> Repeat(true, 2, Some(Some(3)), Dot),
      "(.)" -> Capture(Dot),
      "(?<ab>.)" -> NamedCapture("ab", Dot),
      "(?:.)" -> Group(Dot),
      "(.)\\1" -> Sequence(Seq(Capture(Dot), BackReference(1))),
      "(.)\\2" -> Sequence(Seq(Capture(Dot), Character(2))),
      "(?<ab>.)\\1" -> Sequence(Seq(NamedCapture("ab", Dot), BackReference(1))),
      "(?<ab>.)\\k<ab>" -> Sequence(Seq(NamedCapture("ab", Dot), NamedBackReference("ab"))),
      "(?:.)\\1" -> Sequence(Seq(Group(Dot), Character(1))),
      "(?=.)" -> LookAhead(false, Dot),
      "(?!.)" -> LookAhead(true, Dot),
      "(?<=.)" -> LookBehind(false, Dot),
      "(?<!.)" -> LookBehind(true, Dot),
      "^" -> LineBegin,
      "$" -> LineEnd,
      "\\b" -> WordBoundary(false),
      "\\B" -> WordBoundary(true)
    )

    for (source -> node <- testCases) {
      val result = Parser.parse(source, "")
      assert(result.isDefined)
      assertEquals(result.get.root, node)
      val source2 = result.get.toString
      val result2 = Parser.parse(source2.slice(1, source2.length - 1), "")
      assertEquals(result2, result)
    }
  }

  test("Parser.parse (unicode)") {
    val testCases = Seq(
      "\\u{000A}" -> Character(10),
      "\\p{ID_START}" -> UnicodeProperty(false, "ID_START"),
      "\\p{sc=Hira}" -> UnicodePropertyValue(false, "sc", "Hira"),
      "\\P{ID_START}" -> UnicodeProperty(true, "ID_START"),
      "\\P{sc=Hira}" -> UnicodePropertyValue(true, "sc", "Hira")
    )

    for (source -> node <- testCases) {
      val result = Parser.parse(source, "u")
      assert(result.isDefined)
      assertEquals(result.get.root, node)
      val source2 = result.get.toString
      val result2 = Parser.parse(source2.slice(1, source2.length - 2), "u")
      assertEquals(result2, result)
    }
  }
}
