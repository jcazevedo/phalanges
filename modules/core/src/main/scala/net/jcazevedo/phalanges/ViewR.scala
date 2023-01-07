package net.jcazevedo.phalanges

private[phalanges] sealed abstract class ViewR[V, A] {
  private[phalanges] def fold[B](empty: => B, cons: (Lazy[FingerTree[V, A]], A) => B): B
}

private[phalanges] object ViewR {
  private[phalanges] def empty[V, A]: ViewR[V, A] =
    new ViewR[V, A] {
      def fold[B](empty: => B, cons: (Lazy[FingerTree[V, A]], A) => B): B =
        empty
    }

  private[phalanges] def cons[V, A](tail: Lazy[FingerTree[V, A]], head: A): ViewR[V, A] =
    new ViewR[V, A] {
      def fold[B](empty: => B, cons: (Lazy[FingerTree[V, A]], A) => B): B =
        cons(tail, head)
    }
}
