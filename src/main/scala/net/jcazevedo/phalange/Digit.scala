package net.jcazevedo.phalange

private[phalange] sealed abstract class Digit[V, A](implicit measured: Measured[A, V]) extends Iterable[A] {
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

  def tailOption: Option[Digit[V, A]] =
    fold(
      one = (_, _) => None,
      two = (_, _, a) => Some(Digit(a)),
      three = (_, _, a, b) => Some(Digit(a, b)),
      four = (_, _, a, b, c) => Some(Digit(a, b, c))
    )

  private[phalange] def split(p: V => Boolean, i: V): (Lazy[Option[Digit[V, A]]], A, Lazy[Option[Digit[V, A]]]) = {
    val a = head
    tailOption
      .fold[(Lazy[Option[Digit[V, A]]], A, Lazy[Option[Digit[V, A]]])](((Lazy.pure(None), a, Lazy.pure(None)))) {
        tail =>
          val ni = measured.append(i, measured.apply(a))
          if (p(ni)) (Lazy.pure(None), a, Lazy.pure(Some(tail)))
          else {
            val (l, x, r) = tail.split(p, ni)
            (l.map(_.fold(Some(Digit(a)))(dig => Some(a +: dig))), x, r)
          }
      }
  }

  def +:(a: A): Digit[V, A] =
    fold(
      one = (_, b) => Digit(a, b),
      two = (_, b, c) => Digit(a, b, c),
      three = (_, b, c, d) => Digit(a, b, c, d),
      four = (_, _, _, _, _) => throw new IllegalArgumentException("+: of Digit with size four")
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
        one(Lazy.delay(measured.apply(a)), a)
    }

  private[phalange] def apply[V, A](a: A, b: A)(implicit measured: Measured[A, V]): Digit[V, A] =
    new Digit[V, A] {
      def fold[B](
          one: (Lazy[V], A) => B,
          two: (Lazy[V], A, A) => B,
          three: (Lazy[V], A, A, A) => B,
          four: (Lazy[V], A, A, A, A) => B
      ): B =
        two(Lazy.delay(measured.append(measured.apply(a), measured.apply(b))), a, b)
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
          Lazy.delay(measured.append(measured.apply(a), measured.append(measured.apply(b), measured.apply(c)))),
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
          Lazy.delay(
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
