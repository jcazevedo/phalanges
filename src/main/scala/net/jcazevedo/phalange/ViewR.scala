package net.jcazevedo.phalange

private[phalange] sealed trait ViewR[+A]

private[phalange] object ViewR {
  private[phalange] case object Empty extends ViewR[Nothing]
  private[phalange] case class Cons[+A](tail: Lazy[FingerTree[A]], head: A) extends ViewR[A]
}
