package net.jcazevedo.phalange

private[phalange] sealed trait Node[+A] extends Iterable[A] {
  def iterator: Iterator[A] =
    this match {
      case Node.Node2(a, b)    => Iterator(a, b)
      case Node.Node3(a, b, c) => Iterator(a, b, c)
    }
}

private[phalange] object Node {
  private[phalange] case class Node2[+A](a: A, b: A) extends Node[A]
  private[phalange] case class Node3[+A](a: A, b: A, c: A) extends Node[A]
}
