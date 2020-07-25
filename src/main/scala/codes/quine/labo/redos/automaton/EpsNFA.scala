package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import EpsNFA._

final case class EpsNFA[A, Q](alphabet: Set[A], stateSet: Set[Q], init: Q, accept: Q, tau: Map[Q, Transition[A, Q]]) {
  def toOrderedNFA: OrderedNFA[A, Seq[Q]] = {
    def buildClosure(q: Q, visited: Set[Q]): Seq[Q] =
      if (visited.contains(q)) Seq.empty
      else
        tau.get(q) match {
          case Some(Eps(q1))        => buildClosure(q1, visited + q)
          case Some(Branch(q1, q2)) => buildClosure(q1, visited + q) ++ buildClosure(q2, visited + q)
          case Some(Consume(_, _))  => Seq(q)
          case None                 => Seq(q)
        }
    val closureCache = mutable.Map.empty[Q, Seq[Q]]
    def closure(q: Q): Seq[Q] = closureCache.getOrElseUpdate(q, buildClosure(q, Set.empty))

    val queue = mutable.Queue.empty[Seq[Q]]
    val newStateSet = mutable.Set.empty[Seq[Q]]
    val newInits = Seq(closure(init))
    val newAcceptSet = Set.newBuilder[Seq[Q]]
    val newDelta = Map.newBuilder[(Seq[Q], A), Seq[Seq[Q]]]

    queue.enqueueAll(newInits)
    newStateSet.addAll(newInits)

    while (queue.nonEmpty) {
      val qs = queue.dequeue()
      if (qs.exists(_ == accept)) {
        newAcceptSet.addOne(qs)
      }
      val d = mutable.Map.empty[A, Seq[Seq[Q]]].withDefaultValue(Seq.empty)
      for (q <- qs) {
        tau.get(q) match {
          case Some(Consume(as, q1)) =>
            val qs1 = closure(q1)
            for (a <- as) {
              d(a) = d(a) :+ qs1
            }
            if (!newStateSet.contains(qs1)) {
              queue.enqueue(qs1)
              newStateSet.add(qs1)
            }
          case None => // nothing to do because of terminal state
          case _    => throw new IllegalCallerException
        }
      }
      for (a <- alphabet) {
        newDelta.addOne((qs, a) -> d(a))
      }
    }

    OrderedNFA(alphabet, newStateSet.toSet, newInits, newAcceptSet.result(), newDelta.result())
  }
}

object EpsNFA {
  sealed abstract class Transition[+A, Q] extends Serializable with Product
  final case class Eps[Q](q: Q) extends Transition[Nothing, Q]
  final case class Branch[Q](q1: Q, q2: Q) extends Transition[Nothing, Q]
  final case class Consume[A, Q](as: Set[A], q: Q) extends Transition[A, Q]
}
