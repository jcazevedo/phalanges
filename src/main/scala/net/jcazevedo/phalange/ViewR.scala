package net.jcazevedo.phalange

private[phalange] sealed abstract class ViewR[V, A] {
  private[phalange] def fold[B](empty: => B, cons: (Lazy[FingerTree[V, A]], A) => B): B
}

private[phalange] object ViewR {
  private[phalange] def empty[V, A]: ViewR[V, A] =
    new ViewR[V, A] {
      def fold[B](empty: => B, cons: (Lazy[FingerTree[V, A]], A) => B): B =
        empty
    }

  private[phalange] def cons[V, A](tail: Lazy[FingerTree[V, A]], head: A): ViewR[V, A] =
    new ViewR[V, A] {
      def fold[B](empty: => B, cons: (Lazy[FingerTree[V, A]], A) => B): B =
        cons(tail, head)
    }
}
