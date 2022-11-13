package net.jcazevedo.phalange

trait Measured[-A, V] {
  def apply(a: A): V
}

object Measured {
  private[phalange] implicit def nodeMeasured[A, V]: Measured[Node[V, A], V] =
    new Measured[Node[V, A], V] {
      def apply(a: Node[V, A]): V =
        a match {
          case Node.Node2(v, _, _)    => v
          case Node.Node3(v, _, _, _) => v
        }
    }

  private[phalange] implicit def digitMeasured[A, V](implicit
      measured: Measured[A, V],
      monoid: Monoid[V]
  ): Measured[Digit[A], V] =
    new Measured[Digit[A], V] {
      def apply(a: Digit[A]): V =
        a match {
          case Digit.One(a) =>
            measured.apply(a)

          case Digit.Two(a, b) =>
            monoid.append(measured.apply(a), measured.apply(b))

          case Digit.Three(a, b, c) =>
            monoid.append(monoid.append(measured.apply(a), measured.apply(b)), measured.apply(c))

          case Digit.Four(a, b, c, d) =>
            monoid.append(
              monoid.append(monoid.append(measured.apply(a), measured.apply(b)), measured.apply(c)),
              measured.apply(d)
            )
        }
    }

  private[phalange] implicit def fingerTreeMeasure[A, V](implicit
      measured: Measured[A, V],
      monoid: Monoid[V]
  ): Measured[FingerTree[V, A], V] =
    new Measured[FingerTree[V, A], V] {
      def apply(a: FingerTree[V, A]): V =
        a match {
          case FingerTree.Empty()          => monoid.empty
          case FingerTree.Single(x)        => measured.apply(x)
          case FingerTree.Deep(v, _, _, _) => v.value
        }
    }

  private[phalange] def measure[A, V](a: A)(implicit measured: Measured[A, V]): V =
    measured.apply(a)
}
