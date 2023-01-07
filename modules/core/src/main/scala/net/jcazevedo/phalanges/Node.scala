package net.jcazevedo.phalanges

private[phalanges] sealed abstract class Node[V, A](implicit measured: Measured[A, V]) extends Iterable[A] {
  private[phalanges] def fold[B](node2: (Lazy[V], A, A) => B, node3: (Lazy[V], A, A, A) => B): B

  final lazy val measure: V =
    fold(node2 = (lm, _, _) => lm.value, node3 = (lm, _, _, _) => lm.value)

  final def iterator: Iterator[A] =
    fold(node2 = (_, a, b) => Iterator(a, b), node3 = (_, a, b, c) => Iterator(a, b, c))

  final def toDigit: Digit[V, A] =
    fold(node2 = (_, a, b) => Digit(a, b), node3 = (_, a, b, c) => Digit(a, b, c))
}

private[phalanges] object Node {
  private[phalanges] def apply[V, A](a: A, b: A)(implicit measured: Measured[A, V]): Node[V, A] =
    new Node[V, A] {
      def fold[B](node2: (Lazy[V], A, A) => B, node3: (Lazy[V], A, A, A) => B): B =
        node2(Lazy.delay(measured.append(measured.apply(a), measured.apply(b))), a, b)
    }

  private[phalanges] def apply[V, A](a: A, b: A, c: A)(implicit measured: Measured[A, V]): Node[V, A] =
    new Node[V, A] {
      def fold[B](node2: (Lazy[V], A, A) => B, node3: (Lazy[V], A, A, A) => B): B =
        node3(
          Lazy.delay(measured.append(measured.apply(a), measured.append(measured.apply(b), measured.apply(c)))),
          a,
          b,
          c
        )
    }
}
