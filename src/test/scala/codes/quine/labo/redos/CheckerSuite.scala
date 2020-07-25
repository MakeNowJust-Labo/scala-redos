package codes.quine.labo.redos

import minitest.SimpleTestSuite

import Complexity._
import regexp.RegExp

object CheckerSuite extends SimpleTestSuite {
  test("Checker.check") {
    val Some(constantR) = RegExp.parse("a")
    val Some(linearR1) = RegExp.parse("a*")
    val Some(linearR2) = RegExp.parse("(a|a)*.*")
    val Some(polynomialR1) = RegExp.parse("a*aa*")
    val Some(polynomialR2) = RegExp.parse("a*aa*b*bb*")
    val Some(exponentialR1) = RegExp.parse("(a*)*")
    val Some(exponentialR2) = RegExp.parse("(a|a)*")

    assertEquals(Checker.check(constantR), Constant)
    assertEquals(Checker.check(linearR1), Linear)
    assertEquals(Checker.check(linearR2), Linear)
    assertEquals(Checker.check(polynomialR1), Polynomial(2, Witness(Seq((Seq(), Seq(Some('a')))), Seq(None))))
    assertEquals(
      Checker.check(polynomialR2),
      Polynomial(3, Witness(Seq((Seq(), Seq(Some('a'))), (Seq(Some('b')), Seq(Some('b')))), Seq(Some('a'))))
    )
    assertEquals(Checker.check(exponentialR1), Exponential(Witness(Seq((Seq(Some('a')), Seq(Some('a')))), Seq(None))))
    assertEquals(Checker.check(exponentialR2), Exponential(Witness(Seq((Seq(), Seq(Some('a')))), Seq(None))))
  }
}
