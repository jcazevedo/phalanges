package net.jcazevedo.phalange

private[phalange] sealed trait Node[V, +A] extends Iterable[A] {
  def iterator: Iterator[A] =
    this match {
      case Node.Node2(_, a, b)    => Iterator(a, b)
      case Node.Node3(_, a, b, c) => Iterator(a, b, c)
    }
}

private[phalange] object Node {
  private[phalange] case class Node2[V, +A](v: V, a: A, b: A) extends Node[V, A]
  private[phalange] case class Node3[V, +A](v: V, a: A, b: A, c: A) extends Node[V, A]

  private[phalange] def node2[V, A](a: A, b: A)(implicit measured: Measured[A, V]): Node[V, A] =
    Node.Node2(measured.append(measured.apply(a), measured.apply(b)), a, b)

  private[phalange] def node3[V, A](a: A, b: A, c: A)(implicit measured: Measured[A, V]): Node[V, A] =
    Node.Node3(measured.append(measured.append(measured.apply(a), measured.apply(b)), measured.apply(c)), a, b, c)
}
