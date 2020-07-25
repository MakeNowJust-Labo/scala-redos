package codes.quine.labo.redos

sealed abstract class Complexity[+A] extends Serializable with Product

object Complexity {
  final case object Constant extends Complexity[Nothing]
  final case object Linear extends Complexity[Nothing]
  final case class Polynomial[A](degree: Int, witness: Witness[A]) extends Complexity[A]
  final case class Exponential[A](witness: Witness[A]) extends Complexity[A]
}
