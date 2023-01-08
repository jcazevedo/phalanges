package net.jcazevedo.phalanges.applications

import net.jcazevedo.phalanges.FingerTree
import net.jcazevedo.phalanges.Measured

private[applications] class RandomAccessSequence[A](val self: FingerTree[Int, A]) extends AnyVal {
  def length: Int =
    self.measure

  def splitAt(i: Int): (RandomAccessSequence[A], RandomAccessSequence[A]) = {
    val (l, r) = self.split(_ > i)
    (new RandomAccessSequence(l), new RandomAccessSequence(r))
  }

  def apply(i: Int): A =
    self.dropUntil(_ > i).headL

  def ++(other: RandomAccessSequence[A]): RandomAccessSequence[A] =
    new RandomAccessSequence(self ++ other.self)

  def :+(x: A): RandomAccessSequence[A] =
    new RandomAccessSequence(self :+ x)

  def +:(x: A): RandomAccessSequence[A] =
    new RandomAccessSequence(x +: self)

  def head: A =
    self.headL

  def last: A =
    self.headR

  def tail: RandomAccessSequence[A] =
    new RandomAccessSequence(self.tailL)

  def init: RandomAccessSequence[A] =
    new RandomAccessSequence(self.tailR)

  def drop(n: Int): RandomAccessSequence[A] =
    new RandomAccessSequence(self.split(_ > n)._2)

  def take(n: Int): RandomAccessSequence[A] =
    new RandomAccessSequence(self.split(_ > n)._1)

  def toList: List[A] =
    self.toList
}

object RandomAccessSequence {
  private implicit def sizeMeasured[A]: Measured[A, Int] = new Measured[A, Int] {
    def empty: Int = 0
    def append(a: Int, b: Int): Int = a + b
    def apply(a: A): Int = 1
  }

  def apply[A](xs: A*): RandomAccessSequence[A] =
    new RandomAccessSequence(FingerTree.measured(xs.toSeq: _*))
}
