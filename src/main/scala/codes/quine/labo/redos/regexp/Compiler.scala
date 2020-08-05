package codes.quine.labo.redos
package regexp

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import com.ibm.icu.lang.{UCharacter, UProperty}
import com.ibm.icu.text.{UnicodeSet, UnicodeSetIterator}

import automaton.EpsNFA, EpsNFA._
import Compiler.traverse
import Pattern._

object Compiler {
  def compile(pattern: Pattern): Try[EpsNFA[Option[Int], Int]] = {
    val Pattern(flagSet, node) = pattern
    if (flagSet.ignoreCase || flagSet.multiline || flagSet.unicode) {
      return Failure(new UnsupportedRegExpException("Unsupported flag"))
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

  private def alphabet(node: Node, flagSet: FlagSet): Try[Set[Option[Int]]] =
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
      case Character(c)          => Success(Set(Some(c)))
      case Dot if flagSet.dotAll => Success(Set.empty)
      case Dot                   => Success(Set(Some(0x0a), Some(0x0d)))
      case LineBegin | LineEnd   => Success(Set.empty)
      case _                     => Failure(new UnsupportedRegExpException("Unsupported syntax"))
    }

  private def hasLineBeginAtBegin(node: Node, isBegin: Boolean = true): Try[Boolean] =
    node match {
      case Disjunction(nodes) =>
        for {
          xs <- traverse(nodes)(hasLineBeginAtBegin(_, isBegin))
          x <-
            if (xs.forall(identity(_))) Success(true)
            else if (xs.forall(!identity(_))) Success(false)
            else Failure(new UnsupportedRegExpException("A line begin assertion is mixed with sub matching part."))
        } yield x
      case Sequence(Seq()) => Success(false)
      case Sequence(node +: nodes) =>
        for {
          x <- hasLineBeginAtBegin(node, isBegin)
          _ <- traverse(nodes)(hasLineBeginAtBegin(_, false)).filter(_.forall(!identity(_)))
        } yield x
      case Capture(node)         => hasLineBeginAtBegin(node, isBegin)
      case NamedCapture(_, node) => hasLineBeginAtBegin(node, isBegin)
      case Group(node)           => hasLineBeginAtBegin(node, isBegin)
      case Star(_, node)         => hasLineBeginAtBegin(node, false)
      case Question(_, node)     => hasLineBeginAtBegin(node, false)
      case Plus(_, node)         => hasLineBeginAtBegin(node, false)
      case Repeat(_, _, _, node) => hasLineBeginAtBegin(node, false)
      case LineBegin if isBegin  => Success(true)
      case LineBegin =>
        Failure(new UnsupportedRegExpException("A line begin assertion is not placed at the begin of pattern"))
      case LineEnd | _: Character | Dot => Success(false)
      case _                            => Failure(new UnsupportedRegExpException("Unsupported syntax"))
    }

  private def hasLineEndAtEnd(node: Node, isEnd: Boolean = true): Try[Boolean] =
    node match {
      case Disjunction(nodes) =>
        for {
          xs <- traverse(nodes)(hasLineEndAtEnd(_, isEnd))
          x <-
            if (xs.forall(identity(_))) Success(true)
            else if (xs.forall(!identity(_))) Success(false)
            else Failure(new UnsupportedRegExpException("A line end assertion is mixed with sub matching part."))
        } yield x
      case Sequence(Seq()) => Success(false)
      case Sequence(nodes :+ node) =>
        for {
          _ <- traverse(nodes)(hasLineEndAtEnd(_, false)).filter(_.forall(!identity(_)))
          x <- hasLineEndAtEnd(node, isEnd)
        } yield x
      case Capture(node)         => hasLineEndAtEnd(node, isEnd)
      case NamedCapture(_, node) => hasLineEndAtEnd(node, isEnd)
      case Group(node)           => hasLineEndAtEnd(node, isEnd)
      case Star(_, node)         => hasLineEndAtEnd(node, false)
      case Question(_, node)     => hasLineEndAtEnd(node, false)
      case Plus(_, node)         => hasLineEndAtEnd(node, false)
      case Repeat(_, _, _, node) => hasLineEndAtEnd(node, false)
      case LineEnd if isEnd      => Success(true)
      case LineEnd =>
        Failure(
          new UnsupportedRegExpException("A line end assertion is not placed at the end of pattern")
        )
      case LineBegin | _: Character | Dot => Success(false)
      case _                              => Failure(new UnsupportedRegExpException("Unsupported syntax"))
    }

  def traverse[A, B](xs: Seq[A])(f: A => Try[B]): Try[Seq[B]] = {
    val ys = Seq.newBuilder[B]
    for (x <- xs) {
      f(x) match {
        case Success(y)  => ys.addOne(y)
        case Failure(ex) => return Failure(ex)
      }
    }
    Success(ys.result())
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

  def compile(node: Node): Try[(Int, Int)] =
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
          val q = nextState()
          val accept = nextState()
          tau(init) = Eps(q)
          tau(q) = if (nonGreedy) Branch(accept, i) else Branch(i, accept)
          tau(a) = Eps(q)
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
        Failure(new InvalidRegExpException("Invalid repetition"))
      case Repeat(nonGreedy, min, Some(Some(max)), node) =>
        compile(Sequence(Seq.fill(min)(node) ++ Seq.fill(max - min)(Question(nonGreedy, node))))
      case LineBegin | LineEnd =>
        val q = nextState()
        Success((q, q))
      case Character(c) =>
        val init = nextState()
        val accept = nextState()
        tau(init) = Consume(Set(Some(c)), accept)
        Success((init, accept))
      case Dot =>
        val init = nextState()
        val accept = nextState()
        tau(init) = Consume(if (flagSet.dotAll) alphabet else alphabet.diff(Set(Some(0x0a), Some(0x0d))), accept)
        Success((init, accept))
      case _ => Failure(new UnsupportedRegExpException("Unsupported syntax"))
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

  def subMatch(i: Int, a: Int): (Int, Int) = {
    val (i1, a1) = prefixMatch(i, a)
    suffixMatch(i1, a1)
  }
}
