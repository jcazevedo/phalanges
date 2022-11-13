package net.jcazevedo.phalange

private[phalange] sealed trait ViewR[V, +A]

private[phalange] object ViewR {
  private[phalange] case class Empty[V]() extends ViewR[V, Nothing]
  private[phalange] case class Cons[V, +A](tail: Lazy[FingerTree[V, A]], head: A) extends ViewR[V, A]
}
