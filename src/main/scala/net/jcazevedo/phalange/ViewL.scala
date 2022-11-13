package net.jcazevedo.phalange

private[phalange] sealed trait ViewL[+A]

private[phalange] object ViewL {
  private[phalange] case object Empty extends ViewL[Nothing]
  private[phalange] case class Cons[+A](head: A, tail: Lazy[FingerTree[A]]) extends ViewL[A]
}
