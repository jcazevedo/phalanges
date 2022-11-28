package net.jcazevedo.phalanges

private[phalanges] sealed abstract class ViewL[V, A] {
  private[phalanges] def fold[B](empty: => B, cons: (A, Lazy[FingerTree[V, A]]) => B): B
}

private[phalanges] object ViewL {
  private[phalanges] def empty[V, A]: ViewL[V, A] =
    new ViewL[V, A] {
      def fold[B](empty: => B, cons: (A, Lazy[FingerTree[V, A]]) => B): B =
        empty
    }

  private[phalanges] def cons[V, A](head: A, tail: Lazy[FingerTree[V, A]]): ViewL[V, A] =
    new ViewL[V, A] {
      def fold[B](empty: => B, cons: (A, Lazy[FingerTree[V, A]]) => B): B =
        cons(head, tail)
    }
}
