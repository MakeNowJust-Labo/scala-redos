package codes.quine.labo.redos

import Regex._

sealed abstract class Regex[+A] extends Product with Serializable { self =>
  def toEpsNFA[B >: A]: EpsNFA[Option[B], Int] = {
    var counter = 0
    def newState(): Int = {
      val c = counter
      counter += 1
      c
    }

    def calcAlphabet(r: Regex[A]): Set[B] =
      r match {
        case Singleton(set) => set.toSet
        case Union(r1, r2)  => calcAlphabet(r1) | calcAlphabet(r2)
        case Concat(r1, r2) => calcAlphabet(r1) | calcAlphabet(r2)
        case Star(r, _)     => calcAlphabet(r)
        case _              => Set.empty
      }

    val alphabet = calcAlphabet(this).map(Option(_)) | Set(None)

    def build(r: Regex[A]): (Set[Int], Int, Int, Map[(Int, Option[B]), Seq[Int]], Map[Int, Seq[Int]]) =
      r match {
        case Empty =>
          val q = newState()
          (Set(q), q, q, Map.empty, Map.empty)
        case AnySingleton =>
          val q1 = newState()
          val q2 = newState()
          (Set(q1, q2), q1, q2, alphabet.iterator.map(a => (q1, a) -> Seq(q2)).toMap, Map.empty)
        case Singleton(set) =>
          val q1 = newState()
          val q2 = newState()
          (
            Set(q1, q2),
            q1,
            q2,
            set.iterator.map(a => (q1, Option(a: B)) -> Seq(q2)).toMap,
            Map.empty
          )
        case Union(r1, r2) =>
          val q1 = newState()
          val (stateSet1, init1, accept1, delta1, deltaEps1) = build(r1)
          val (stateSet2, init2, accept2, delta2, deltaEps2) = build(r2)
          val q2 = newState()
          val stateSet = stateSet1 | stateSet2 | Set(q1, q2)
          val delta = delta1 ++ delta2
          val deltaEps = deltaEps1 ++ deltaEps2 ++ Map(q1 -> Seq(init1, init2), accept1 -> Seq(q2), accept2 -> Seq(q2))
          (stateSet, q1, q2, delta, deltaEps)
        case Concat(r1, r2) =>
          val (stateSet1, init1, accept1, delta1, deltaEps1) = build(r1)
          val (stateSet2, init2, accept2, delta2, deltaEps2) = build(r2)
          val stateSet = stateSet1 | stateSet2
          val delta = delta1 ++ delta2
          val deltaEps = deltaEps1 ++ deltaEps2 ++ Map(accept1 -> Seq(init2))
          (stateSet, init1, accept2, delta, deltaEps)
        case Star(r, greedy) =>
          val q1 = newState()
          val (stateSet, init, accept, delta, deltaEps) = build(r)
          val q2 = newState()
          val newStateSet = stateSet | Set(q1, q2)
          val loop = if (greedy) Seq(init, q2) else Seq(q2, init)
          val newDeltaEps = deltaEps ++ Map(q1 -> loop, accept -> loop)
          (newStateSet, q1, q2, delta, newDeltaEps)
      }

    val (stateSet, init, accept, delta, deltaEps) = build(this)
    EpsNFA(alphabet, stateSet, init, accept, delta, deltaEps)
  }
}

object Regex {
  case object Empty extends Regex[Nothing]
  case object AnySingleton extends Regex[Nothing]
  final case class Singleton[A](set: Set[A]) extends Regex[A]
  final case class Union[A](r1: Regex[A], r2: Regex[A]) extends Regex[A]
  final case class Concat[A](r1: Regex[A], r2: Regex[A]) extends Regex[A]
  final case class Star[A](r: Regex[A], greedy: Boolean) extends Regex[A]

  def parse(string: String): Option[Regex[Char]] = {
    def union(string: Seq[Char]): Option[(Regex[Char], Seq[Char])] = {
      var s = string

      val rs = Seq.newBuilder[Regex[Char]]
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

    def concat(string: Seq[Char]): Option[(Regex[Char], Seq[Char])] = {
      var s = string
      if (s.isEmpty || s.head == ')') return Some((Empty, s))

      val rs = Seq.newBuilder[Regex[Char]]
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

    def star(string: Seq[Char]): Option[(Regex[Char], Seq[Char])] =
      atom(string) match {
        case Some((r, s)) if s.startsWith("*?") => Some((Star(r, false), s.drop(2)))
        case Some((r, s)) if s.startsWith("*")  => Some((Star(r, true), s.tail))
        case Some((r, s))                       => Some((r, s))
        case _                                  => None
      }

    def atom(string: Seq[Char]): Option[(Regex[Char], Seq[Char])] =
      string match {
        case '(' +: s =>
          union(s) match {
            case Some((r, s)) if s.headOption == Some(')') => Some((r, s.tail))
            case _                                         => None
          }
        case '.' +: s       => Some((AnySingleton, s))
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
