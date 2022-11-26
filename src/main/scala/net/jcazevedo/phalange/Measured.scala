package net.jcazevedo.phalange

trait Measured[-A, V] extends Monoid[V] {
  def apply(a: A): V
}

object Measured {
  private[phalange] implicit def nodeMeasured[A, V](implicit monoid: Monoid[V]): Measured[Node[V, A], V] =
    new Measured[Node[V, A], V] {
      def apply(a: Node[V, A]): V = a.measure
      def empty: V = monoid.empty
      def append(a: V, b: V) = monoid.append(a, b)
    }
}
