package codes.quine.labo.redos

import scala.util.Success
import scala.util.Try

import minitest.SimpleTestSuite

import Complexity._
import automaton.EpsNFA
import regexp.{Parser, Compiler}

object CheckerSuite extends SimpleTestSuite {
  def parse(s: String): Try[EpsNFA[Option[Int], Int]] =
    for {
      pattern <- Parser.parse(s, "")
      epsNFA <- Compiler.compile(pattern)
    } yield epsNFA

  test("Checker.check") {
    val Success(constantNFA) = parse("^a$")
    val Success(linearNFA1) = parse("^a*$")
    val Success(linearNFA2) = parse("^(a|a)*")
    val Success(polynomialNFA1) = parse("^a*aa*$")
    val Success(polynomialNFA2) = parse("^a*aa*b*bb*$")
    val Success(exponentialNFA1) = parse("^(a*)*$")
    val Success(exponentialNFA2) = parse("^(a|a)*$")

    assertEquals(Checker.check(constantNFA), Constant)
    assertEquals(Checker.check(linearNFA1), Linear)
    assertEquals(Checker.check(linearNFA2), Linear)
    assertEquals(Checker.check(polynomialNFA1), Polynomial(2, Witness(Seq((Seq(), Seq(Some('a'.toInt)))), Seq(None))))
    assertEquals(
      Checker.check(polynomialNFA2),
      Polynomial(
        3,
        Witness(Seq((Seq(), Seq(Some('a'.toInt))), (Seq(Some('b'.toInt)), Seq(Some('b'.toInt)))), Seq(None))
      )
    )
    assertEquals(
      Checker.check(exponentialNFA1),
      Exponential(Witness(Seq((Seq(Some('a'.toInt)), Seq(Some('a'.toInt)))), Seq(None)))
    )
    assertEquals(Checker.check(exponentialNFA2), Exponential(Witness(Seq((Seq(), Seq(Some('a'.toInt)))), Seq(None))))
  }
}
