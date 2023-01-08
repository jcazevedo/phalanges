package net.jcazevedo.phalanges.applications

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop.forAll

class MaxPriorityQueueSpec extends Specification with ScalaCheck {
  "A MaxPriorityQueue" should {
    def toOrderedList[A](pq: MaxPriorityQueue[A]): List[A] =
      if (pq.isEmpty) Nil
      else {
        val (h, t) = pq.dequeue
        h :: toOrderedList(t)
      }

    "support dequeuing according to priority" in forAll { ints: List[Int] =>
      val pq = MaxPriorityQueue(ints: _*)
      toOrderedList(pq) ==== ints.sorted.reverse
    }

    "support an enqueue method" in forAll { (toEnqueue: List[Int], existing: List[Int]) =>
      val pq = toEnqueue.foldLeft(MaxPriorityQueue(existing: _*))(_.enqueue(_))
      toOrderedList(pq) ==== (existing ++ toEnqueue).sorted.reverse
    }
  }
}