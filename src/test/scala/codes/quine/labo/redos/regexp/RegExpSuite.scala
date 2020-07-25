package codes.quine.labo.redos
package regexp

import minitest.SimpleTestSuite

import automaton.EpsNFA, EpsNFA._
import RegExp._

object RegExpSuite extends SimpleTestSuite {
  test("RegExp.parse") {
    val testCases = Seq(
      "" -> Empty(),
      "a" -> Singleton(Set('a')),
      "[ab]" -> Singleton(Set('a', 'b')),
      "." -> AnySingleton(),
      "ab" -> Concat(Singleton(Set('a')), Singleton(Set('b'))),
      "a|b" -> Union(Singleton(Set('a')), Singleton(Set('b'))),
      "a*" -> Star(Singleton(Set('a')), true),
      "a*?" -> Star(Singleton(Set('a')), false)
    )

    for (string -> expected <- testCases) {
      assertEquals(RegExp.parse(string), Some(expected))
    }
  }

  test("RegExp#toEpsNFA") {
    val testCases = Seq(
      Empty() -> EpsNFA(Set[Option[Char]](None), Set(0), 0, 0, Map.empty[Int, Transition[Option[Char], Int]]),
      Singleton(Set('a')) -> EpsNFA(Set(Some('a'), None), Set(0, 1), 0, 1, Map(0 -> Consume(Set(Some('a')), 1))),
      AnySingleton() -> EpsNFA(Set(None), Set(0, 1), 0, 1, Map(0 -> Consume(Set(None), 1))),
      Singleton(Set('a', 'b')) ->
        EpsNFA(Set(Some('a'), Some('b'), None), Set(0, 1), 0, 1, Map(0 -> Consume(Set(Some('a'), Some('b')), 1))),
      Union(Singleton(Set('a')), Singleton(Set('b'))) ->
        EpsNFA(
          Set(Some('a'), Some('b'), None),
          Set(0, 1, 2, 3, 4, 5),
          0,
          5,
          Map(
            0 -> Branch(1, 3),
            1 -> Consume(Set(Some('a')), 2),
            3 -> Consume(Set(Some('b')), 4),
            2 -> Eps(5),
            4 -> Eps(5)
          )
        ),
      Concat(Singleton(Set('a')), Singleton(Set('b'))) ->
        EpsNFA(
          Set(Some('a'), Some('b'), None),
          Set(0, 1, 2, 3),
          0,
          3,
          Map(0 -> Consume(Set(Some('a')), 1), 1 -> Eps(2), 2 -> Consume(Set(Some('b')), 3))
        ),
      Star(Singleton(Set('a')), true) ->
        EpsNFA(
          Set(Some('a'), None),
          Set(0, 1, 2, 3),
          0,
          3,
          Map(0 -> Branch(1, 3), 1 -> Consume(Set(Some('a')), 2), 2 -> Branch(1, 3))
        ),
      Star(Singleton(Set('a')), false) ->
        EpsNFA(
          Set(Some('a'), None),
          Set(0, 1, 2, 3),
          0,
          3,
          Map(0 -> Branch(3, 1), 1 -> Consume(Set(Some('a')), 2), 2 -> Branch(3, 1))
        )
    )

    for (r -> epsNFA <- testCases) {
      assertEquals(r.toEpsNFA, epsNFA)
    }
  }
}
