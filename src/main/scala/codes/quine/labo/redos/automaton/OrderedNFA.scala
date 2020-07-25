package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import util.Graph

final case class OrderedNFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    inits: Seq[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), Seq[Q]]
) {
  def toGraph: Graph[Q, A] = Graph.from(delta.iterator.flatMap { case (q1, a) -> qs => qs.map((q1, a, _)) }.toSeq)

  def rename: OrderedNFA[A, Int] = {
    val f = stateSet.zipWithIndex.toMap
    OrderedNFA(
      alphabet,
      f.values.toSet,
      inits.map(f),
      acceptSet.map(f),
      delta.map { case (q1, a) -> qs => (f(q1), a) -> qs.map(f) }
    )
  }

  def reverse: NFA[A, Q] = {
    val reverseDelta = mutable.Map.empty[(Q, A), Set[Q]].withDefaultValue(Set.empty)
    for ((q1, a) -> qs <- delta; q2 <- qs) {
      reverseDelta((q2, a)) = reverseDelta((q2, a)) | Set(q1)
    }
    NFA(alphabet, stateSet, acceptSet, inits.toSet, reverseDelta.toMap)
  }
}
