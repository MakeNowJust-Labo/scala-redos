package codes.quine.labo.redos

import minitest.SimpleTestSuite

import Complexity._
import automaton.EpsNFA
import regexp.{Parser, Compiler}

object CheckerSuite extends SimpleTestSuite {
  def parse(s: String): Option[EpsNFA[Option[Int], Int]] =
    for {
      pattern <- Parser.parse(s, "")
      epsNFA <- Compiler.compile(pattern)
    } yield epsNFA

  test("Checker.check") {
    val Some(constantNFA) = parse("^a$")
    val Some(linearNFA1) = parse("^a*$")
    val Some(linearNFA2) = parse("^(a|a)*")
    val Some(polynomialNFA1) = parse("^a*aa*$")
    val Some(polynomialNFA2) = parse("^a*aa*b*bb*$")
    val Some(exponentialNFA1) = parse("^(a*)*$")
    val Some(exponentialNFA2) = parse("^(a|a)*$")

    assertEquals(Checker.check(constantNFA), Constant)
    assertEquals(Checker.check(linearNFA1), Linear)
    assertEquals(Checker.check(linearNFA2), Linear)
    assertEquals(Checker.check(polynomialNFA1), Polynomial(2, Witness(Seq((Seq(), Seq(Some('a'.toInt)))), Seq(None))))
    assertEquals(
      Checker.check(polynomialNFA2),
      Polynomial(3, Witness(Seq((Seq(), Seq(Some('a'.toInt))), (Seq(Some('b'.toInt)), Seq(Some('b'.toInt)))), Seq(None)))
    )
    assertEquals(Checker.check(exponentialNFA1), Exponential(Witness(Seq((Seq(Some('a'.toInt)), Seq(Some('a'.toInt)))), Seq(None))))
    assertEquals(Checker.check(exponentialNFA2), Exponential(Witness(Seq((Seq(), Seq(Some('a'.toInt)))), Seq(None))))
  }
}
