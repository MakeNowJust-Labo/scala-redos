package codes.quine.labo.redos
package automaton

import minitest.SimpleTestSuite

import EpsNFA._

object EpsNFASuite extends SimpleTestSuite {
  test("EpsNFA#toOrderedNFA") {
    val tau: Map[Int, Transition[Char, Int]] = Map(
      0 -> Branch(1, 2),
      1 -> Consume(Set('a'), 5),
      2 -> Branch(3, 4),
      3 -> Consume(Set('a'), 5),
      4 -> Consume(Set('b'), 5)
    )
    val epsNFA = EpsNFA(Set('a', 'b'), Set(0, 1, 2), 0, 5, tau)
    val nfa = epsNFA.toOrderedNFA

    assertEquals(nfa.alphabet, epsNFA.alphabet)

    assertEquals(nfa.stateSet, Set(Seq(1, 3, 4), Seq(5)))
    assertEquals(nfa.inits, Seq(Seq(1, 3, 4)))
    assertEquals(nfa.acceptSet, Set(Seq(5)))
    assertEquals(
      nfa.delta,
      Map(
        (Seq(1, 3, 4), 'a') -> Seq(Seq(5), Seq(5)),
        (Seq(1, 3, 4), 'b') -> Seq(Seq(5)),
        (Seq(5), 'a') -> Seq(),
        (Seq(5), 'b') -> Seq()
      )
    )
  }
}
