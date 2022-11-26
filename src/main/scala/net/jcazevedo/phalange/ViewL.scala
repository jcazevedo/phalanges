package net.jcazevedo.phalange

private[phalange] sealed abstract class ViewL[V, A] {
  private[phalange] def fold[B](empty: => B, cons: (A, Lazy[FingerTree[V, A]]) => B): B
}

private[phalange] object ViewL {
  private[phalange] def empty[V, A]: ViewL[V, A] =
    new ViewL[V, A] {
      def fold[B](empty: => B, cons: (A, Lazy[FingerTree[V, A]]) => B): B =
        empty
    }

  private[phalange] def cons[V, A](head: A, tail: Lazy[FingerTree[V, A]]): ViewL[V, A] =
    new ViewL[V, A] {
      def fold[B](empty: => B, cons: (A, Lazy[FingerTree[V, A]]) => B): B =
        cons(head, tail)
    }
}
