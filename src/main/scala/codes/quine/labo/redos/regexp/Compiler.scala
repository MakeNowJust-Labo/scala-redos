package codes.quine.labo.redos
package regexp

import scala.collection.mutable
import scala.util.Try

import com.ibm.icu.lang.{UCharacter, UProperty}
import com.ibm.icu.text.{UnicodeSet, UnicodeSetIterator}

import automaton.EpsNFA, EpsNFA._
import Compiler.traverse
import Pattern._

object Compiler {
  def compile(pattern: Pattern): Option[EpsNFA[Option[Int], Int]] = {
    val Pattern(flagSet, node) = pattern
    if (flagSet.ignoreCase || flagSet.multiline || flagSet.unicode) {
      return None // unsupported
    }
    for {
      alphabet <- alphabet(node, flagSet)
      compiler = new Compiler(flagSet, alphabet + None)
      (init0, accept0) <- compiler.compile(pattern.root)
      hasLineBeginAtBegin <- hasLineBeginAtBegin(node)
      hasLineEndAtEnd <- hasLineEndAtEnd(node)
      (init, accept) = (hasLineBeginAtBegin, hasLineEndAtEnd) match {
        case (true, true)   => (init0, accept0)
        case (true, false)  => compiler.prefixMatch(init0, accept0)
        case (false, true)  => compiler.suffixMatch(init0, accept0)
        case (false, false) => compiler.subMatch(init0, accept0)
      }
    } yield EpsNFA(compiler.alphabet, compiler.stateSet.toSet, init, accept, compiler.tau.toMap)
  }

  private def alphabet(node: Node, flagSet: FlagSet): Option[Set[Option[Int]]] =
    node match {
      case Disjunction(nodes) =>
        traverse(nodes)(alphabet(_, flagSet)).map(_.iterator.flatten.toSet)
      case Sequence(nodes) =>
        traverse(nodes)(alphabet(_, flagSet)).map(_.iterator.flatten.toSet)
      case Capture(node)         => alphabet(node, flagSet)
      case NamedCapture(_, node) => alphabet(node, flagSet)
      case Group(node)           => alphabet(node, flagSet)
      case Star(_, node)         => alphabet(node, flagSet)
      case Question(_, node)     => alphabet(node, flagSet)
      case Plus(_, node)         => alphabet(node, flagSet)
      case Repeat(_, _, _, node) => alphabet(node, flagSet)
      case Character(c)          => Some(Set(Some(c)))
      case Dot if flagSet.dotAll => Some(Set.empty)
      case Dot                   => Some(Set(Some(0x0a), Some(0x0d)))
      case LineBegin | LineEnd   => Some(Set.empty)
      case _                     => None // unsupported
    }

  private def hasLineBeginAtBegin(node: Node, isBegin: Boolean = true): Option[Boolean] =
    node match {
      case Disjunction(nodes) =>
        traverse(nodes)(hasLineBeginAtBegin(_, isBegin)).map(_.forall(identity(_)))
      case Sequence(Seq()) => Some(false)
      case Sequence(node +: nodes) =>
        for {
          x <- hasLineBeginAtBegin(node, isBegin)
          _ <- traverse(nodes)(hasLineBeginAtBegin(_, false))
        } yield x
      case Capture(node)                => hasLineBeginAtBegin(node, isBegin)
      case NamedCapture(_, node)        => hasLineBeginAtBegin(node, isBegin)
      case Group(node)                  => hasLineBeginAtBegin(node, isBegin)
      case Star(_, node)                => hasLineBeginAtBegin(node, false)
      case Question(_, node)            => hasLineBeginAtBegin(node, false)
      case Plus(_, node)                => hasLineBeginAtBegin(node, false)
      case Repeat(_, _, _, node)        => hasLineBeginAtBegin(node, false)
      case LineBegin if isBegin         => Some(true)
      case LineBegin                    => None // LineBegin at invalid place
      case LineEnd | _: Character | Dot => Some(false)
      case _                            => None // unsupported
    }

  private def hasLineEndAtEnd(node: Node, isEnd: Boolean = true): Option[Boolean] =
    node match {
      case Disjunction(nodes) =>
        traverse(nodes)(hasLineEndAtEnd(_, isEnd)).map(_.forall(identity(_)))
      case Sequence(Seq()) => Some(false)
      case Sequence(nodes :+ node) =>
        for {
          _ <- traverse(nodes)(hasLineEndAtEnd(_, false))
          x <- hasLineEndAtEnd(node, isEnd)
        } yield x
      case Capture(node)                  => hasLineEndAtEnd(node, isEnd)
      case NamedCapture(_, node)          => hasLineEndAtEnd(node, isEnd)
      case Group(node)                    => hasLineEndAtEnd(node, isEnd)
      case Star(_, node)                  => hasLineEndAtEnd(node, false)
      case Question(_, node)              => hasLineEndAtEnd(node, false)
      case Plus(_, node)                  => hasLineEndAtEnd(node, false)
      case Repeat(_, _, _, node)          => hasLineEndAtEnd(node, false)
      case LineEnd if isEnd               => Some(true)
      case LineEnd                        => None // LineEnd at invalid place
      case LineBegin | _: Character | Dot => Some(false)
      case _                              => None // unsupported
    }

  def traverse[A, B](xs: Seq[A])(f: A => Option[B]): Option[Seq[B]] = {
    val ys = Seq.newBuilder[B]
    for (x <- xs) {
      f(x) match {
        case Some(y) => ys.addOne(y)
        case None    => return None
      }
    }
    Some(ys.result())
  }
}

final private class Compiler(val flagSet: Pattern.FlagSet, val alphabet: Set[Option[Int]]) {
  val stateSet = mutable.Set.empty[Int]
  val tau = mutable.Map.empty[Int, Transition[Option[Int], Int]]

  private def nextState(): Int = {
    val state = stateSet.size
    stateSet.add(state)
    state
  }

  def compile(node: Node): Option[(Int, Int)] =
    node match {
      case Disjunction(nodes) =>
        for {
          ias <- traverse(nodes)(compile(_))
        } yield {
          val init = ias.map(_._1).reduceRight { (i1, i2) =>
            val i = nextState()
            tau(i) = Branch(i1, i2)
            i
          }
          val accept = nextState()
          ias.map(_._2).foreach(tau(_) = Eps(accept))
          (init, accept)
        }
      case Sequence(nodes) =>
        for {
          ias <- traverse(nodes)(compile(_))
        } yield ias
          .reduceLeftOption[(Int, Int)] {
            case ((i1, a1), (i2, a2)) =>
              tau(a1) = Eps(i2)
              (i1, a2)
          }
          .getOrElse {
            val q = nextState()
            (q, q)
          }
      case Capture(node)         => compile(node)
      case NamedCapture(_, node) => compile(node)
      case Group(node)           => compile(node)
      case Star(nonGreedy, node) =>
        for {
          (i, a) <- compile(node)
        } yield {
          val init = nextState()
          val accept = nextState()
          val loop = if (nonGreedy) Branch(accept, i) else Branch(i, accept)
          tau(init) = loop
          tau(a) = loop
          (init, accept)
        }
      case Question(nonGreedy, node) =>
        for {
          (i, a) <- compile(node)
        } yield {
          val init = nextState()
          val accept = nextState()
          tau(init) = if (nonGreedy) Branch(accept, i) else Branch(i, accept)
          tau(a) = Eps(accept)
          (init, accept)
        }
      case Plus(nonGreedy, node) =>
        compile(Sequence(Seq(node, Star(nonGreedy, node))))
      case Repeat(_, n, None, node) =>
        compile(Sequence(Seq.fill(n)(node)))
      case Repeat(nonGreedy, min, Some(None), node) =>
        compile(Sequence(Seq.fill(min)(node) :+ Star(nonGreedy, node)))
      case Repeat(_, min, Some(Some(max)), _) if min > max =>
        None
      case Repeat(nonGreedy, min, Some(Some(max)), node) =>
        compile(Sequence(Seq.fill(min)(node) ++ Seq.fill(max - min)(Question(nonGreedy, node))))
      case LineBegin | LineEnd =>
        val q = nextState()
        Some((q, q))
      case Character(c) =>
        val init = nextState()
        val accept = nextState()
        tau(init) = Consume(Set(Some(c)), accept)
        Some((init, accept))
      case Dot =>
        val init = nextState()
        val accept = nextState()
        tau(init) = Consume(if (flagSet.dotAll) alphabet else alphabet.diff(Set(Some(0x0a), Some(0x0d))), accept)
        Some((init, accept))
      case _ => None // unsupported
    }

  def suffixMatch(i: Int, a: Int): (Int, Int) = {
    val init = nextState()
    val q = nextState()
    tau(init) = Branch(i, q)
    tau(q) = Consume(alphabet, init)
    (init, a)
  }

  def prefixMatch(i: Int, a: Int): (Int, Int) = {
    val q = nextState()
    val accept = nextState()
    tau(a) = Branch(q, accept)
    tau(q) = Consume(alphabet, a)
    (i, accept)
  }

  def subMatch(i: Int, a: Int): (Int, Int) =
    prefixMatch(i, a) match {
      case (i1, a1) => suffixMatch(i1, a1)
    }
}
