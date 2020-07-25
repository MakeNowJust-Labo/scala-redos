package codes.quine.labo.redos
package automaton

import minitest.SimpleTestSuite

object NFASuite extends SimpleTestSuite {
  test("NFA#toDFA") {
    val nfa = NFA(Set('a', 'b'), Set(0, 1), Set(0), Set(1), Map((0, 'a') -> Set(0, 1), (1, 'b') -> Set(1)))
    val dfa = nfa.toDFA

    assertEquals(dfa.alphabet, nfa.alphabet)
    assert(dfa.stateSet.subsetOf(nfa.stateSet.subsets().toSet))
    assertEquals(dfa.init, nfa.initSet)
    assertEquals(dfa.acceptSet, dfa.stateSet.filter(qs => (qs & nfa.acceptSet).nonEmpty))
    for ((qs1, a) -> qs2 <- dfa.delta; q1 <- qs1) {
      assert(nfa.delta.getOrElse((q1, a), Set.empty).subsetOf(qs2))
    }

    assertEquals(
      dfa.delta,
      Map(
        (Set(), 'a') -> Set(),
        (Set(), 'b') -> Set(),
        (Set(0), 'a') -> Set(0, 1),
        (Set(0), 'b') -> Set(),
        (Set(1), 'a') -> Set(),
        (Set(1), 'b') -> Set(1),
        (Set(0, 1), 'a') -> Set(0, 1),
        (Set(0, 1), 'b') -> Set(1)
      )
    )
  }
}
