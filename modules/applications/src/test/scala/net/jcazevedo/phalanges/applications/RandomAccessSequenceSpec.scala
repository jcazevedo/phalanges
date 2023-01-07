package net.jcazevedo.phalanges.applications

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop.forAll

class RandomAccessSequenceSpec extends Specification with ScalaCheck {
  "A RandomAccessSequence" should {
    "support an apply method" in forAll { ints: List[Int] =>
      RandomAccessSequence(ints: _*).toList ==== ints
    }

    "support a length method" in forAll { ints: List[Int] =>
      RandomAccessSequence(ints: _*).length ==== ints.length
    }

    "support a splitAt method" in forAll { ints: List[Int] =>
      val seq = RandomAccessSequence(ints: _*)
      forall(0 to ints.length)(n => {
        val (seqL, seqR) = seq.splitAt(n)
        val (intsL, intsR) = ints.splitAt(n)
        seqL.toList ==== intsL
        seqR.toList ==== intsR
      })
    }

    "support an apply method" in forAll { ints: List[Int] =>
      val seq = RandomAccessSequence(ints: _*)
      forall(0 until ints.length)(i => seq(i) ==== ints(i))
    }

    "support a concat method" in forAll { (ints1: List[Int], ints2: List[Int]) =>
      val seq1 = RandomAccessSequence(ints1: _*)
      val seq2 = RandomAccessSequence(ints2: _*)
      (seq1 ++ seq2).toList ==== ints1 ++ ints2
    }

    "support a prepend method" in forAll { (toPrepend: List[Int], existing: List[Int]) =>
      val seq = toPrepend.foldRight(RandomAccessSequence(existing: _*))(_ +: _)
      seq.toList ==== toPrepend ++ existing
    }

    "support an append method" in forAll { (existing: List[Int], toAppend: List[Int]) =>
      val seq = toAppend.foldLeft(RandomAccessSequence(existing: _*))(_ :+ _)
      seq.toList ==== existing ++ toAppend
    }

    "support a head method" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        RandomAccessSequence(ints: _*).head ==== ints.head
      }
    }

    "support a last method" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        RandomAccessSequence(ints: _*).last ==== ints.last
      }
    }

    "support a tail method" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        RandomAccessSequence(ints: _*).tail.toList ==== ints.tail
      }
    }

    "support an init method" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        RandomAccessSequence(ints: _*).init.toList ==== ints.init
      }
    }

    "support a drop method" in forAll { ints: List[Int] =>
      val seq = RandomAccessSequence(ints: _*)
      forall(0 to ints.length)(n => seq.drop(n).toList ==== ints.drop(n))
    }

    "support a take method" in forAll { ints: List[Int] =>
      val seq = RandomAccessSequence(ints: _*)
      forall(0 to ints.length)(n => seq.take(n).toList ==== ints.take(n))
    }
  }
}
