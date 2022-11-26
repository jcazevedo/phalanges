package net.jcazevedo.phalange

private[phalange] sealed abstract class Node[V, +A] extends Iterable[A] {
  private[phalange] def fold[B](node2: (Lazy[V], A, A) => B, node3: (Lazy[V], A, A, A) => B): B

  def measure: V =
    fold(node2 = (lm, _, _) => lm.value, node3 = (lm, _, _, _) => lm.value)

  def iterator: Iterator[A] =
    fold(node2 = (_, a, b) => Iterator(a, b), node3 = (_, a, b, c) => Iterator(a, b, c))
}

private[phalange] object Node {
  private[phalange] def apply[V, A](a: A, b: A)(implicit measured: Measured[A, V]): Node[V, A] =
    new Node[V, A] {
      def fold[B](node2: (Lazy[V], A, A) => B, node3: (Lazy[V], A, A, A) => B): B =
        node2(new Lazy(measured.append(measured.apply(a), measured.apply(b))), a, b)
    }

  private[phalange] def apply[V, A](a: A, b: A, c: A)(implicit measured: Measured[A, V]): Node[V, A] =
    new Node[V, A] {
      def fold[B](node2: (Lazy[V], A, A) => B, node3: (Lazy[V], A, A, A) => B): B =
        node3(
          new Lazy(measured.append(measured.apply(a), measured.append(measured.apply(b), measured.apply(c)))),
          a,
          b,
          c
        )
    }
}
