package codes.quine.labo.redos

import scala.collection.mutable

import Checker._

object Checker {
  sealed trait Complexity[+A]
  final case object Constant extends Complexity[Nothing]
  final case object Linear extends Complexity[Nothing]
  final case class Polynomial[A](degree: Int, witness: Witness[A]) extends Complexity[A]
  final case class Exponential[A](witness: Witness[A]) extends Complexity[A]

  final case class Witness[A](pump: Seq[(Seq[A], Seq[A])], suffix: Seq[A])

  def check[A](r: Regex[A]): Complexity[Option[A]] =
    new Checker(r).check()

  def decompose[A, Q](nfa: NFA[A, Q], reverseDFA: DFA[A, Set[Q]]): NFA[A, (Q, Set[Q])] = {
    val NFA(alphabet, stateSet, inits, acceptSet, delta) = nfa

    val reverseDelta = reverseDFA.delta.groupMap(_._1._2) { case (p2, _) -> p1 => (p1, p2) }.withDefaultValue(Seq.empty)

    val newStateSet = for (q <- stateSet; p <- reverseDFA.stateSet) yield (q, p)
    val newInits = for (q <- inits; p <- reverseDFA.stateSet) yield (q, p)
    val newAcceptSet = for (q <- reverseDFA.init) yield (q, reverseDFA.init)

    val newDelta = mutable.Map.empty[((Q, Set[Q]), A), Seq[(Q, Set[Q])]].withDefaultValue(Seq.empty)
    for ((q1, a) -> qs <- delta) {
      for ((p1, p2) <- reverseDelta(a)) {
        val qps = qs
          .zip(qs.inits.toSeq.reverse)
          .filterNot { case (_, qs) => qs.exists(p2.contains(_)) }
          .map(qqs => (qqs._1, p2))
        newDelta(((q1, p1), a)) = newDelta(((q1, p1), a)) ++ qps
      }
    }

    NFA(alphabet, newStateSet, newInits, newAcceptSet, newDelta.toMap)
  }
}

final class Checker[A](private[this] val r: Regex[A]) {
  val nfa = r.toEpsNFA.toNFA.rename
  val reverseDFA = nfa.reverse.toDFA
  val decomposedNFA = Checker.decompose(nfa, reverseDFA)

  val graph = decomposedNFA.toGraph.reachableFrom(decomposedNFA.inits.toSet)
  val scc = graph.scc
  val sccMap = (for (sc <- scc; q <- sc) yield q -> sc).toMap
  val sccGraph = Graph.from(
    graph.edges
      .map { case (q1, _, q2) => (sccMap(q1), (), sccMap(q2)) }
      .filter { case (sc1, _, sc2) => sc1 != sc2 }
      .distinct
  )

  val psccMap = scc.map(sc => sc -> sc.map(_._2).toSet).toMap

  val sccReachableMap = sccGraph.reachableMap
  val sccReverseReachableMap = sccGraph.reverse.reachableMap
  val sccPairEdges = graph.edges
    .groupMap { case (q1, a, q2) => (sccMap(q1), sccMap(q2)) } { case (q1, a, q2) => a -> (q1, q2) }
    .view
    .mapValues(_.groupMap(_._1)(_._2).withDefaultValue(Seq.empty))
    .toMap
    .withDefaultValue(Map.empty.withDefaultValue(Seq.empty))

  type Q = (Int, Set[Int])
  type Pump = (Q, Seq[Option[A]], Q)

  def isAtom(sc: Seq[Q]): Boolean =
    sc.size == 1 && !graph.adj(sc.head).exists(_._2 == sc.head)

  def check(): Complexity[Option[A]] =
    checkExponential() match {
      case Some(pump) => Exponential(witness(Seq(pump)))
      case None =>
        checkPolynomial() match {
          case (0, _)          => Constant
          case (1, _)          => Linear
          case (degree, pumps) => Polynomial(degree, witness(pumps))
        }
    }

  def checkExponential(): Option[Pump] = {
    scc.iterator.filterNot(isAtom(_)).flatMap(checkExponentialComponent(_)).nextOption()
  }

  def checkExponentialComponent(sc: Seq[Q]): Option[Pump] = {
    val edges = sccPairEdges((sc, sc))

    edges.find { case (_, es) => es.size != es.distinct.size } match {
      case Some((a, es)) =>
        for {
          (q1, q2) <- es.diff(es.distinct).headOption
          back <- graph.path(Set(q2), q1)
        } yield (q1, a +: back, q1)
      case None =>
        val g2 = Graph.from(for {
          a1 -> es <- edges.toSeq
          (q11, q12) <- es
          (q21, q22) <- es
        } yield ((q11, q21), a1, (q12, q22)))
        g2.scc.iterator
          .flatMap { sc =>
            for {
              p1 <- sc.find { case (q1, q2) => q1 != q2 }
              if sc.exists { case (q1, q2) => q1 == q2 } && sc.exists { case (q1, q2) => q1 != q2 }
              p2 <- sc.find { case (q1, q2) => p1._1 == q1 && p1._1 == q2 }
              path1 <- g2.path(Set(p2), p1)
              path2 <- g2.path(Set(p1), p2)
            } yield (p1._1, path1 ++ path2, p1._1)
          }
          .nextOption()
    }
  }

  def checkPolynomial(): (Int, Seq[Pump]) =
    scc.map(checkPolynomialComponent(_)).maxBy(_._1)

  private[this] val checkPolynomialComponentCache = mutable.Map.empty[Seq[Q], (Int, Seq[Pump])]

  def checkPolynomialComponent(sc: Seq[Q]): (Int, Seq[Pump]) =
    checkPolynomialComponentCache.getOrElseUpdate(
      sc, {
        val (maxDegree, maxPumps) =
          sccGraph.adj(sc).map(usc => checkPolynomialComponent(usc._2)).maxByOption(_._1).getOrElse((0, Seq.empty))
        if (maxDegree == 0) (if (isAtom(sc)) 0 else 1, Seq.empty)
        else if (isAtom(sc)) (maxDegree, maxPumps)
        else {
          sccReachableMap(sc).iterator
            .filter(target =>
              sc != target && !isAtom(target) && checkPolynomialComponent(target)._1 == maxDegree &&
                (psccMap(sc) & psccMap(target)).nonEmpty
            )
            .flatMap(target => checkPolynomialComponentBetween(sc, target).map((target, _)))
            .map { case (target, pump) => (maxDegree + 1, pump +: checkPolynomialComponent(target)._2) }
            .nextOption()
            .getOrElse((maxDegree, maxPumps))
        }
      }
    )

  def checkPolynomialComponentBetween(source: Seq[Q], target: Seq[Q]): Option[Pump] = {
    val sourceEdges = sccPairEdges((source, source))
    val between = sccReachableMap(source) & sccReverseReachableMap(target)
    val betweenEdges = for (sc1 <- between; sc2 <- between) yield sccPairEdges((sc1, sc2))
    val targetEdges = sccPairEdges((target, target))

    val g3 = Graph.from(
      (for {
        a <- decomposedNFA.alphabet.iterator
        (q11, q12) <- sourceEdges(a)
        (q21, q22) <- betweenEdges.flatMap(_(a))
        (q31, q32) <- targetEdges(a)
      } yield ((q11, q21, q31), a, (q12, q22, q32))).toSeq
    )
    val g3back = Graph.from(
      (for {
        q1 <- source
        q2 <- target
      } yield ((q1, q2, q2), None, (q1, q1, q2))) ++
        g3.edges.map { case (qqq1, a, qqq2) => (qqq1, Some(a), qqq2) }
    )

    g3back.scc.iterator
      .flatMap(sc =>
        sc.collect {
          case (q1, q2, q3) if q1 == q2 && q2 != q3 => (q1, q3)
        }.find {
          case (q11, q12) =>
            sc.exists {
              case (q21, q22, q23) => q21 == q11 && q22 == q12 && q23 == q12
            }
        }.flatMap {
          case (q1, q2) =>
            for {
              path <- g3.path(Set((q1, q1, q2)), (q1, q2, q2))
            } yield (q1, path, q2)
        }
      )
      .nextOption()
  }

  def witness(pumps: Seq[Pump]): Witness[Option[A]] = {
    val (pumpPaths, qs) = pumps.foldLeft((Seq.empty[(Seq[Option[A]], Seq[Option[A]])], decomposedNFA.inits.toSet)) {
      case ((pumpPaths, last), (q1, path, q2)) =>
        val prefix = graph.path(last, q1).get
        (pumpPaths :+ (prefix, path), Set(q2))
    }
    val suffix = reverseDFA.toGraph.path(Set(reverseDFA.init), qs.head._2).get.reverse
    Witness(pumpPaths, suffix)
  }
}