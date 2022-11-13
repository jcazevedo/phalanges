package net.jcazevedo.phalange

private[phalange] sealed trait ViewL[V, +A]

private[phalange] object ViewL {
  private[phalange] case class Empty[V]() extends ViewL[V, Nothing]
  private[phalange] case class Cons[V, +A](head: A, tail: Lazy[FingerTree[V, A]]) extends ViewL[V, A]
}
