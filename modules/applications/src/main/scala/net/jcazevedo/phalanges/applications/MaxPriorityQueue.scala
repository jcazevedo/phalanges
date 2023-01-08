package net.jcazevedo.phalanges.applications

import net.jcazevedo.phalanges.FingerTree
import net.jcazevedo.phalanges.Measured
import scala.math.Ordering.Implicits._

private[applications] class MaxPriorityQueue[A: Ordering](val self: FingerTree[MaxPriorityQueue.Priority[A], A]) {
  def enqueue(x: A): MaxPriorityQueue[A] =
    new MaxPriorityQueue(x +: self)

  def dequeue: (A, MaxPriorityQueue[A]) = {
    val (l, x, r) = self.splitTree(_ >= self.measure, MaxPriorityQueue.MinusInfinity)
    (x, new MaxPriorityQueue(l.value ++ r.value))
  }

  def isEmpty: Boolean =
    self.isEmpty

  def head: A =
    self.splitTree(_ >= self.measure, MaxPriorityQueue.MinusInfinity)._2
}

object MaxPriorityQueue {
  private[applications] sealed trait Priority[+A]
  private[applications] case object MinusInfinity extends Priority[Nothing]
  private[applications] case class ValuePriority[A](value: A) extends Priority[A]

  private[applications] object Priority {
    implicit def ordering[A: Ordering]: Ordering[Priority[A]] = new Ordering[Priority[A]] {
      def compare(x: Priority[A], y: Priority[A]): Int = (x, y) match {
        case (MinusInfinity, _) => -1
        case (_, MinusInfinity) => 1
        case (ValuePriority(a), ValuePriority(b)) if a == b => 0
        case (ValuePriority(a), ValuePriority(b)) if a < b => -1
        case (ValuePriority(_), ValuePriority(_)) => 1
      }
    }
  }

  private implicit def priorityMeasured[A](implicit ord: Ordering[A]): Measured[A, Priority[A]] =
    new Measured[A, Priority[A]] {
      def empty: Priority[A] = MinusInfinity
      def append(a: Priority[A], b: Priority[A]): Priority[A] = a.max(b)
      def apply(a: A): Priority[A] = ValuePriority(a)
    }

  def apply[A: Ordering](xs: A*): MaxPriorityQueue[A] =
    new MaxPriorityQueue(FingerTree.measured(xs.toSeq: _*))
}
