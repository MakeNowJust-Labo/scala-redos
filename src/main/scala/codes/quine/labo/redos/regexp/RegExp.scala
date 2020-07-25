package codes.quine.labo.redos
package regexp

import automaton._, EpsNFA._
import RegExp._

sealed abstract class RegExp[A] extends Product with Serializable { self =>
  def toEpsNFA: EpsNFA[Option[A], Int] = {
    var counter = 0
    def newState(): Int = {
      val c = counter
      counter += 1
      c
    }

    def calcAlphabet(r: RegExp[A]): Set[A] =
      r match {
        case Singleton(set) => set
        case Union(r1, r2)  => calcAlphabet(r1) | calcAlphabet(r2)
        case Concat(r1, r2) => calcAlphabet(r1) | calcAlphabet(r2)
        case Star(r, _)     => calcAlphabet(r)
        case _              => Set.empty
      }

    val alphabet = calcAlphabet(this).map(Option(_)) | Set(None)

    def build(r: RegExp[A]): EpsNFA[Option[A], Int] =
      r match {
        case Empty() =>
          val q = newState()
          EpsNFA(alphabet, Set(q), q, q, Map.empty)
        case AnySingleton() =>
          val q1 = newState()
          val q2 = newState()
          EpsNFA(alphabet, Set(q1, q2), q1, q2, Map(q1 -> Consume(alphabet, q2)))
        case Singleton(set) =>
          val q1 = newState()
          val q2 = newState()
          EpsNFA(alphabet, Set(q1, q2), q1, q2, Map(q1 -> Consume(set.map(Option(_)), q2)))
        case Union(r1, r2) =>
          val q1 = newState()
          val EpsNFA(_, stateSet1, init1, accept1, delta1) = build(r1)
          val EpsNFA(_, stateSet2, init2, accept2, delta2) = build(r2)
          val q2 = newState()
          val stateSet = stateSet1 | stateSet2 | Set(q1, q2)
          val delta = delta1 ++ delta2 ++ Map(q1 -> Branch(init1, init2), accept1 -> Eps(q2), accept2 -> Eps(q2))
          EpsNFA(alphabet, stateSet, q1, q2, delta)
        case Concat(r1, r2) =>
          val EpsNFA(_, stateSet1, init1, accept1, delta1) = build(r1)
          val EpsNFA(_, stateSet2, init2, accept2, delta2) = build(r2)
          val stateSet = stateSet1 | stateSet2
          val delta = delta1 ++ delta2 ++ Map(accept1 -> Eps(init2))
          EpsNFA(alphabet, stateSet, init1, accept2, delta)
        case Star(r, greedy) =>
          val q1 = newState()
          val EpsNFA(_, stateSet, init, accept, delta) = build(r)
          val q2 = newState()
          val newStateSet = stateSet | Set(q1, q2)
          val loop = if (greedy) Branch(init, q2) else Branch(q2, init)
          val newDelta = delta ++ Map(q1 -> loop, accept -> loop)
          EpsNFA(alphabet, newStateSet, q1, q2, newDelta)
      }

    build(this)
  }
}

object RegExp {
  final case class Empty[A]() extends RegExp[A]
  final case class AnySingleton[A]() extends RegExp[A]
  final case class Singleton[A](set: Set[A]) extends RegExp[A]
  final case class Union[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
  final case class Concat[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
  final case class Star[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]

  def parse(string: String): Option[RegExp[Char]] = {
    def union(string: Seq[Char]): Option[(RegExp[Char], Seq[Char])] = {
      var s = string

      val rs = Seq.newBuilder[RegExp[Char]]
      while (true) {
        concat(s) match {
          case Some((r, s1)) if s1.isEmpty || "|)".contains(s1.head) =>
            rs.addOne(r)
            s = s1
          case _ => return None
        }

        if (s.isEmpty || s.head == ')') {
          return Some((rs.result().reduceRight(Union(_, _)), s))
        }

        // s.head must be '|' here.
        s = s.tail
      }

      ???
    }

    def concat(string: Seq[Char]): Option[(RegExp[Char], Seq[Char])] = {
      var s = string
      if (s.isEmpty || s.head == ')') return Some((Empty(), s))

      val rs = Seq.newBuilder[RegExp[Char]]
      do {
        star(s) match {
          case Some((r, s1)) =>
            rs.addOne(r)
            s = s1
          case _ => return None
        }
      } while (!(s.isEmpty || "|)".contains(s.head)))

      Some((rs.result().reduceRight(Concat(_, _)), s))
    }

    def star(string: Seq[Char]): Option[(RegExp[Char], Seq[Char])] =
      atom(string) match {
        case Some((r, s)) if s.startsWith("*?") => Some((Star(r, false), s.drop(2)))
        case Some((r, s)) if s.startsWith("*")  => Some((Star(r, true), s.tail))
        case Some((r, s))                       => Some((r, s))
        case _                                  => None
      }

    def atom(string: Seq[Char]): Option[(RegExp[Char], Seq[Char])] =
      string match {
        case '(' +: s =>
          union(s) match {
            case Some((r, s)) if s.headOption == Some(')') => Some((r, s.tail))
            case _                                         => None
          }
        case '.' +: s       => Some((AnySingleton(), s))
        case '\\' +: a +: s => Some((Singleton(Set(a)), s))
        case '[' +: s =>
          charSet(s) match {
            case Some((as, s)) => Some((Singleton(as), s))
            case _             => None
          }
        case a +: s if "*?()".contains(a) => None
        case a +: s                       => Some((Singleton(Set(a)), s))
        case _                            => None
      }

    def charSet(string: Seq[Char]): Option[(Set[Char], Seq[Char])] = {
      var s = string
      val set = Set.newBuilder[Char]

      while (true) {
        s match {
          case ']' +: s1 => return Some((set.result(), s1))
          case a +: s1 =>
            set.addOne(a)
            s = s1
          case _ => return None
        }
      }

      ???
    }

    union(string.toSeq) match {
      case Some((r, Seq())) => Some(r)
      case _                => None
    }
  }
}
