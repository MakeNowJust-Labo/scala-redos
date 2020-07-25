package codes.quine.labo.redos
package automaton

import minitest.SimpleTestSuite

object OrderedNFASuite extends SimpleTestSuite {
  test("OrderedNFA#reverse") {
    val nfa = OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Seq(0, 1, 0), (1, 'b') -> Seq(1)))
    val reverseNFA = nfa.reverse

    assertEquals(reverseNFA.alphabet, nfa.alphabet)
    assertEquals(reverseNFA.stateSet, nfa.stateSet)
    assertEquals(reverseNFA.initSet, nfa.acceptSet)
    assertEquals(reverseNFA.acceptSet, nfa.inits.toSet)
    assertEquals(reverseNFA.toGraph.edges.toSet, nfa.toGraph.reverse.edges.toSet)
  }

  test("OrderedNFA#rename") {
    val nfa1 = OrderedNFA(
      Set('a', 'b'),
      Set("0", "1"),
      Seq("0"),
      Set("1"),
      Map(("0", 'a') -> Seq("0", "1", "0"), ("1", 'b') -> Seq("1"))
    )
    val nfa2 = nfa1.rename

    assertEquals(nfa2.alphabet, nfa1.alphabet)
    assertEquals(nfa2.stateSet, Set(0, 1))
    assertEquals(nfa2.inits.size, 1)
    assertEquals(nfa2.acceptSet.size, 1)
    val f = Map("0" -> nfa2.inits.head, "1" -> nfa2.acceptSet.head)
    assertEquals(nfa2.delta, nfa1.delta.map { case (q, a) -> qs => (f(q), a) -> qs.map(f) })
  }
}
