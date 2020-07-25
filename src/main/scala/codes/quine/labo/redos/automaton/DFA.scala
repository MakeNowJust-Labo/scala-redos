package codes.quine.labo.redos
package automaton

import util.Graph

final case class DFA[A, Q](alphabet: Set[A], stateSet: Set[Q], init: Q, acceptSet: Set[Q], delta: Map[(Q, A), Q]) {
  def toGraph: Graph[Q, A] = Graph.from(delta.iterator.map { case (q1, a) -> q2 => (q1, a, q2) }.toSeq)
}
