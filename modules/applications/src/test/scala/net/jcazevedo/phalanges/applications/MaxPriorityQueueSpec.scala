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

    "support dequeueing according to priority" in forAll { ints: List[Int] =>
      val pq = MaxPriorityQueue(ints: _*)
      toOrderedList(pq) ==== ints.sorted.reverse
    }
  }
}
