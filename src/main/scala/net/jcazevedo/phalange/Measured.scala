package net.jcazevedo.phalange

trait Measured[-A, V] extends Monoid[V] {
  def apply(a: A): V
}

object Measured {
  private[phalange] implicit def nodeMeasured[A, V](implicit monoid: Monoid[V]): Measured[Node[V, A], V] =
    new Measured[Node[V, A], V] {
      def apply(a: Node[V, A]): V =
        a match {
          case Node.Node2(v, _, _)    => v
          case Node.Node3(v, _, _, _) => v
        }

      def empty: V =
        monoid.empty

      def append(a: V, b: V) =
        monoid.append(a, b)
    }

  private[phalange] implicit def digitMeasured[A, V](implicit measured: Measured[A, V]): Measured[Digit[A], V] =
    new Measured[Digit[A], V] {
      def apply(a: Digit[A]): V =
        a match {
          case Digit.One(a) =>
            measured.apply(a)

          case Digit.Two(a, b) =>
            append(measured.apply(a), measured.apply(b))

          case Digit.Three(a, b, c) =>
            append(append(measured.apply(a), measured.apply(b)), measured.apply(c))

          case Digit.Four(a, b, c, d) =>
            append(append(append(measured.apply(a), measured.apply(b)), measured.apply(c)),measured.apply(d))
        }

      def empty: V =
        measured.empty

      def append(a: V, b: V) =
        measured.append(a, b)
    }

  private[phalange] implicit def fingerTreeMeasure[A, V](implicit measured: Measured[A, V]): Measured[FingerTree[V, A], V] =
    new Measured[FingerTree[V, A], V] {
      def apply(a: FingerTree[V, A]): V =
        a match {
          case FingerTree.Empty()          => empty
          case FingerTree.Single(x)        => measured.apply(x)
          case FingerTree.Deep(v, _, _, _) => v.value
        }

      def empty: V =
        measured.empty

      def append(a: V, b: V): V =
        measured.append(a, b)
    }

  private[phalange] def measure[A, V](a: A)(implicit measured: Measured[A, V]): V =
    measured.apply(a)
}
