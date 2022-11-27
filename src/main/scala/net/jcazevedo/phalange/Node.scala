package net.jcazevedo.phalange

private[phalange] sealed abstract class Node[V, A](implicit measured: Measured[A, V]) extends Iterable[A] {
  private[phalange] def fold[B](node2: (V, A, A) => B, node3: (V, A, A, A) => B): B

  final def measure: V =
    fold(node2 = (m, _, _) => m, node3 = (m, _, _, _) => m)

  final def iterator: Iterator[A] =
    fold(node2 = (_, a, b) => Iterator(a, b), node3 = (_, a, b, c) => Iterator(a, b, c))

  final def toDigit: Digit[V, A] =
    fold(node2 = (_, a, b) => Digit(a, b), node3 = (_, a, b, c) => Digit(a, b, c))
}

private[phalange] object Node {
  private[phalange] def apply[V, A](a: A, b: A)(implicit measured: Measured[A, V]): Node[V, A] =
    new Node[V, A] {
      def fold[B](node2: (V, A, A) => B, node3: (V, A, A, A) => B): B =
        node2(measured.append(measured.apply(a), measured.apply(b)), a, b)
    }

  private[phalange] def apply[V, A](a: A, b: A, c: A)(implicit measured: Measured[A, V]): Node[V, A] =
    new Node[V, A] {
      def fold[B](node2: (V, A, A) => B, node3: (V, A, A, A) => B): B =
        node3(
          measured.append(measured.apply(a), measured.append(measured.apply(b), measured.apply(c))),
          a,
          b,
          c
        )
    }
}
