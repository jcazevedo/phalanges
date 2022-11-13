package net.jcazevedo.phalange

import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class FingerTreeSpec extends Specification with ScalaCheck {
  "A FingerTree" should {
    "support an apply method" in forAll { ints: List[Int] =>
      val ft = FingerTree.apply(ints: _*)
      ft.toList ==== ints
    }

    "support a foldRight operation" in forAll { ints: List[Int] =>
      val ft = FingerTree.apply(ints: _*)
      ft.foldRight(List.empty[Int])(_ :: _) ==== ints
    }

    "support a foldLeft operation" in forAll { ints: List[Int] =>
      val ft = FingerTree.apply(ints: _*)
      ft.foldLeft(0L)(_ + _.toLong) ==== ints.map(_.toLong).sum
    }

    "support a prepend operation" in {
      val ft = 1 +: 2 +: 3 +: 4 +: 5 +: FingerTree.empty[Int]
      ft.toList ==== List(1, 2, 3, 4, 5)
    }

    "support an append operation" in {
      val ft = FingerTree.empty[Int] :+ 1 :+ 2 :+ 3 :+ 4 :+ 5
      ft.toList ==== List(1, 2, 3, 4, 5)
    }

    "support a headL operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.headL ==== ints.head
      }
    }

    "support a headL operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.headL must throwA[NoSuchElementException]
    }

    "support a headLOption operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.headLOption ==== Some(ints.head)
      }
    }

    "support a headLOption operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.headLOption ==== None
    }

    "support a tailL operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.tailL.toList ==== ints.tail
      }
    }

    "support a tailL operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.tailL must throwA[NoSuchElementException]
    }

    "support a tailLOption operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.tailLOption must beSome.which(_.toList ==== ints.tail)
      }
    }

    "support a tailLOption operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.tailLOption ==== None
    }

    "support a headR operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.headR ==== ints.last
      }
    }

    "support a headR operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.headR must throwA[NoSuchElementException]
    }

    "support a headROption operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.headROption ==== Some(ints.last)
      }
    }

    "support a headROption operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.headROption ==== None
    }

    "support a tailR operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.tailR.toList ==== ints.init
      }
    }

    "support a tailR operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.tailR must throwA[NoSuchElementException]
    }

    "support a tailROption operation in non-empty trees" in forAll { ints: List[Int] =>
      ints.nonEmpty ==> {
        val ft = FingerTree.apply(ints: _*)
        ft.tailROption must beSome.which(_.toList ==== ints.init)
      }
    }

    "support a tailROption operation in empty trees" in {
      val ft = FingerTree.empty[Int]
      ft.tailROption ==== None
    }

    "support concatentation" in forAll { (xs: List[Int], ys: List[Int]) =>
      val ft1 = FingerTree.apply(xs: _*)
      val ft2 = FingerTree.apply(ys: _*)
      val resultFt = ft1 ++ ft2
      val resultList = xs ++ ys

      resultFt.foldRight(List.empty[Int])(_ :: _) ==== resultList
      resultFt.foldLeft(0L)(_ + _.toLong) ==== resultList.map(_.toLong).sum
    }
  }
}
