package net.jcazevedo.phalange

private[phalange] sealed trait Digit[+A] extends Iterable[A] {
  def iterator: Iterator[A] =
    this match {
      case Digit.One(a)           => Iterator(a)
      case Digit.Two(a, b)        => Iterator(a, b)
      case Digit.Three(a, b, c)   => Iterator(a, b, c)
      case Digit.Four(a, b, c, d) => Iterator(a, b, c, d)
    }
}

private[phalange] object Digit {
  private[phalange] case class One[A](a: A) extends Digit[A]
  private[phalange] case class Two[A](a: A, b: A) extends Digit[A]
  private[phalange] case class Three[A](a: A, b: A, c: A) extends Digit[A]
  private[phalange] case class Four[A](a: A, b: A, c: A, d: A) extends Digit[A]
}
