package codes.quine.labo.redos

final case class Witness[A](pump: Seq[(Seq[A], Seq[A])], suffix: Seq[A])
