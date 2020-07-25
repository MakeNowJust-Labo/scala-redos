package codes.quine.labo.redos
package automaton

import scala.collection.MultiSet

import util.Graph

final case class MultiNFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    initSet: MultiSet[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), MultiSet[Q]]
) {
  def toGraph: Graph[Q, A] = Graph.from(delta.iterator.flatMap { case (q1, a) -> qs => qs.map((q1, a, _)) }.toSeq)
}
