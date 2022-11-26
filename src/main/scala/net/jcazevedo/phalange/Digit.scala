package net.jcazevedo.phalange

private[phalange] sealed abstract class Digit[V, A] extends Iterable[A] {
  private[phalange] def fold[B](
      one: (Lazy[V], A) => B,
      two: (Lazy[V], A, A) => B,
      three: (Lazy[V], A, A, A) => B,
      four: (Lazy[V], A, A, A, A) => B
  ): B

  final def measure: V =
    fold(
      one = (lm, _) => lm.value,
      two = (lm, _, _) => lm.value,
      three = (lm, _, _, _) => lm.value,
      four = (lm, _, _, _, _) => lm.value
    )

  final def iterator: Iterator[A] =
    fold(
      one = (_, a) => Iterator(a),
      two = (_, a, b) => Iterator(a, b),
      three = (_, a, b, c) => Iterator(a, b, c),
      (_, a, b, c, d) => Iterator(a, b, c, d)
    )
}

private[phalange] object Digit {
  private[phalange] def apply[V, A](a: A)(implicit measured: Measured[A, V]): Digit[V, A] =
    new Digit[V, A] {
      def fold[B](
          one: (Lazy[V], A) => B,
          two: (Lazy[V], A, A) => B,
          three: (Lazy[V], A, A, A) => B,
          four: (Lazy[V], A, A, A, A) => B
      ): B =
        one(new Lazy(measured.apply(a)), a)
    }

  private[phalange] def apply[V, A](a: A, b: A)(implicit measured: Measured[A, V]): Digit[V, A] =
    new Digit[V, A] {
      def fold[B](
          one: (Lazy[V], A) => B,
          two: (Lazy[V], A, A) => B,
          three: (Lazy[V], A, A, A) => B,
          four: (Lazy[V], A, A, A, A) => B
      ): B =
        two(new Lazy(measured.append(measured.apply(a), measured.apply(b))), a, b)
    }

  private[phalange] def apply[V, A](a: A, b: A, c: A)(implicit measured: Measured[A, V]): Digit[V, A] =
    new Digit[V, A] {
      def fold[B](
          one: (Lazy[V], A) => B,
          two: (Lazy[V], A, A) => B,
          three: (Lazy[V], A, A, A) => B,
          four: (Lazy[V], A, A, A, A) => B
      ): B =
        three(
          new Lazy(measured.append(measured.apply(a), measured.append(measured.apply(b), measured.apply(c)))),
          a,
          b,
          c
        )
    }

  private[phalange] def apply[V, A](a: A, b: A, c: A, d: A)(implicit measured: Measured[A, V]): Digit[V, A] =
    new Digit[V, A] {
      def fold[B](
          one: (Lazy[V], A) => B,
          two: (Lazy[V], A, A) => B,
          three: (Lazy[V], A, A, A) => B,
          four: (Lazy[V], A, A, A, A) => B
      ): B =
        four(
          new Lazy(
            measured.append(
              measured.apply(a),
              measured.append(measured.apply(b), measured.append(measured.apply(c), measured.apply(d)))
            )
          ),
          a,
          b,
          c,
          d
        )
    }
}
