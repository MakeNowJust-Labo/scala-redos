package codes.quine.labo.redos

import scala.collection.mutable
import scala.sys.process.Process

final case class DFA[A, Q](alphabet: Set[A], stateSet: Set[Q], init: Q, acceptSet: Set[Q], delta: Map[(Q, A), Q]) {
  def toGraph: Graph[Q, A] = Graph.from(delta.map { case ((q1, a), q2) => (q1, a, q2) }.toSeq)
}

final case class NFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    inits: Seq[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), Seq[Q]]
) {
  def rename: NFA[A, Int] = {
    val mapping = mutable.Map.empty[Q, Int]
    def rename(q: Q): Int = mapping.getOrElseUpdate(q, mapping.size)
    val newStateSet = stateSet.map(rename(_))
    val newInits = inits.map(rename(_))
    val newAcceptSet = acceptSet.map(rename(_))
    val newDelta = delta.map { case (q, a) -> qs => (rename(q), a) -> qs.map(rename(_)) }
    NFA(alphabet, newStateSet, newInits, newAcceptSet, newDelta)
  }

  def reverse: NFA[A, Q] = {
    val newDelta = delta.toSeq.flatMap { case (q1, a) -> qs => qs.map(q2 => ((q2, a), q1)) }.groupMap(_._1)(_._2)
    NFA(alphabet, stateSet, acceptSet.toSeq, inits.toSet, newDelta)
  }

  def toDFA: DFA[A, Set[Q]] = {
    val queue = mutable.Queue.empty[Set[Q]]
    val newStateSet = mutable.Set.empty[Set[Q]]
    val newInit = inits.toSet
    val newAcceptSet = Set.newBuilder[Set[Q]]
    val newDelta = Map.newBuilder[(Set[Q], A), Set[Q]]

    queue.enqueue(newInit)
    newStateSet.add(newInit)

    while (queue.nonEmpty) {
      val qs = queue.dequeue()
      if ((qs & acceptSet).nonEmpty) {
        newAcceptSet.addOne(qs)
      }
      val d = mutable.Map.empty[A, Set[Q]].withDefaultValue(Set.empty)
      for (a <- alphabet) {
        d(a) = qs.flatMap(q => delta.getOrElse((q, a), Set.empty))
      }
      for ((a, qs2) <- d) {
        newDelta.addOne((qs, a) -> qs2)
        if (!newStateSet.contains(qs2)) {
          queue.enqueue(qs2)
          newStateSet.add(qs2)
        }
      }
    }

    DFA(alphabet, newStateSet.toSet, newInit, newAcceptSet.result(), newDelta.result())
  }

  def toGraph: Graph[Q, A] =
    Graph.from(delta.flatMap { case (q1, a) -> qs => qs.map((q1, a, _)) }.toSeq)
}

final case class EpsNFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    init: Q,
    accept: Q,
    delta: Map[(Q, A), Seq[Q]],
    deltaEps: Map[Q, Seq[Q]]
) {
  def toNFA: NFA[A, Seq[Q]] = {
    def buildPathClosure(q: Q, path: Seq[Q]): Seq[Seq[Q]] =
      if (path.contains(q)) Seq.empty
      else {
        val isTerminalState =
          deltaEps.get(q).forall(_.isEmpty) ||
            alphabet.exists(a => delta.get((q, a)).exists(_.nonEmpty)) ||
            q == accept
        val paths =
          if (isTerminalState) Seq(path :+ q)
          else Seq.empty
        paths ++ deltaEps.getOrElse(q, Seq.empty).toSeq.flatMap { q2 => buildPathClosure(q2, path :+ q) }
      }
    val pathClosureCache = mutable.Map.empty[Q, Seq[Seq[Q]]]
    def pathClosure(q: Q): Seq[Seq[Q]] = pathClosureCache.getOrElseUpdate(q, buildPathClosure(q, Seq.empty))

    val queue = mutable.Queue.empty[Seq[Q]]
    val newStateSet = mutable.Set.empty[Seq[Q]]
    val newInitSet = Seq(pathClosure(init).map(_.last))
    val newAcceptSet = Set.newBuilder[Seq[Q]]
    val newDelta = Map.newBuilder[(Seq[Q], A), Seq[Seq[Q]]]

    queue.enqueueAll(newInitSet)
    newStateSet.addAll(newInitSet)

    while (queue.nonEmpty) {
      val qs = queue.dequeue()
      if (qs.exists(_ == accept)) {
        newAcceptSet.addOne(qs)
      }
      for (a <- alphabet) {
        val qs2 = qs.flatMap(q => delta.getOrElse((q, a), Seq.empty).map(pathClosure(_).map(_.last)))
        newDelta.addOne((qs, a) -> qs2)
        val newqs = qs2.filterNot(newStateSet.contains(_))
        queue.enqueueAll(newqs)
        newStateSet.addAll(newqs)
      }
    }

    NFA(alphabet, newStateSet.toSet, newInitSet, newAcceptSet.result(), newDelta.result())
  }

  def toGraph: Graph[Q, Option[A]] = {
    val deltaEdges = delta.iterator.flatMap { case (q1, a) -> qs => qs.map((q1, Some(a), _)) }
    val deltaEpsEdges = deltaEps.iterator.flatMap { case q1 -> qs => qs.map((q1, None, _)) }
    Graph.from((deltaEdges ++ deltaEpsEdges).toSeq)
  }
}
