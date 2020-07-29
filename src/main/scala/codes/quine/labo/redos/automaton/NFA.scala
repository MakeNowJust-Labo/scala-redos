package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import util.{Graph, GraphRenderer}, GraphRenderer._

final case class NFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    initSet: Set[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), Set[Q]]
) {
  def toGraph: Graph[Q, A] = Graph.from(delta.iterator.flatMap { case (q1, a) -> qs => qs.map((q1, a, _)) }.toSeq)

  def toDFA: DFA[A, Set[Q]] = {
    val queue = mutable.Queue.empty[Set[Q]]
    val newStateSet = mutable.Set.empty[Set[Q]]
    val newAcceptSet = Set.newBuilder[Set[Q]]
    val newDelta = Map.newBuilder[(Set[Q], A), Set[Q]]

    queue.enqueue(initSet)
    newStateSet.add(initSet)

    while (queue.nonEmpty) {
      val qs = queue.dequeue()
      if ((qs & acceptSet).nonEmpty) {
        newAcceptSet.addOne(qs)
      }
      for (a <- alphabet) {
        val qs2 = qs.flatMap(q => delta.getOrElse((q, a), Set.empty))
        newDelta.addOne((qs, a) -> qs2)
        if (!newStateSet.contains(qs2)) {
          queue.enqueue(qs2)
          newStateSet.add(qs2)
        }
      }
    }

    DFA(alphabet, newStateSet.toSet, initSet, newAcceptSet.result(), newDelta.result())
  }
}

object NFA {
  implicit def GraphRendererInstance[A: AlphabetLabel, Q: StateLabel]: GraphRenderer[NFA[A, Q]] =
    GraphRenderer.automaton(_.toGraph, _.initSet.contains(_), _.acceptSet.contains(_))
}
