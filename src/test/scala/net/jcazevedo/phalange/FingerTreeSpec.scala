package net.jcazevedo.phalange

import org.specs2.mutable._
import org.specs2.specification.AfterExample

class FingerTreeSpec extends Specification {
  "A Finger Tree" should {
    "support fold right operation" in {
      val f =
        Deep(
          Digit('t', 'h'),
          Deep(
            Digit(
              Node('i', 's'),
              Node('i', 's')
            ),
            Empty,
            Digit(
              Node('n', 'o', 't'),
              Node('a', 't')
            )
          ),
          Digit('r', 'e', 'e')
        )

      val l = f.foldRight(List[Char]()) { (c, l) =>
        c :: l
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support fold left operation" in {
      val f =
        Deep(
          Digit('t', 'h'),
          Deep(
            Digit(
              Node('i', 's'),
              Node('i', 's')
            ),
            Empty,
            Digit(
              Node('n', 'o', 't'),
              Node('a', 't')
            )
          ),
          Digit('r', 'e', 'e')
        )

      val l = f.foldLeft(List[Char]()) { (l, c) =>
        l ++ List(c)
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support cons operation" in {
      val f = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty

      f.toString mustEqual "thisisnotatree"
    }

    "support snoc operation" in {
      val f = Empty + 't' + 'h' + 'i' + 's' + 'i' + 's' + 'n' + 'o' + 't' + 'a' + 't' + 'r' + 'e' + 'e'

      f.toString mustEqual "thisisnotatree"
    }

    "support cons and snoc operations intertwined" in {
      val f = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: Empty + 'o' + 't' + 'a' + 't' + 'r' + 'e' + 'e'

      f.toString mustEqual "thisisnotatree"
    }

    "support toList method" in {
      val f = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty
      val l = f.toList

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support toTree method" in {
      import FingerTree.Implicits._

      val l = List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
      val f: FingerTree[Char] = l
      val fl = f.toList

      fl mustEqual l
    }

    "support toTree method in digits" in {
      val d1 = Digit('t')
      val d2 = Digit('t', 'h')
      val d3 = Digit('t', 'h', 'i')
      val d4 = Digit('t', 'h', 'i', 's')

      d1.toTree.toString mustEqual "t"
      d2.toTree.toString mustEqual "th"
      d3.toTree.toString mustEqual "thi"
      d4.toTree.toString mustEqual "this"
    }

    "support toDigit method in nodes" in {
      val n2 = Node('a', 'b')
      val n3 = Node('a', 'b', 'c')

      n2.toDigit mustEqual Two('a', 'b')
      n3.toDigit mustEqual Three('a', 'b', 'c')
    }

    "support viewL method" in {
      val f1 = Empty
      val f2 = 't' :: Empty
      val f3 = 't' :: 'h' :: Empty
      val f4 = 't' :: 'h' :: 'i' :: Empty
      val f5 = 't' :: 'h' :: 'i' :: 's' :: Empty

      f1.viewL mustEqual None
      f2.viewL mustEqual Some(('t', Empty))
      f3.viewL mustEqual Some(('t', Single('h')))
      f4.viewL mustEqual Some(('t', Deep(One('h'), Empty, One('i'))))
      f5.viewL mustEqual Some(('t', Deep(Two('h', 'i'), Empty, One('s'))))
    }

    "support viewR method" in {
      val f1 = Empty
      val f2 = 't' :: Empty
      val f3 = 't' :: 'h' :: Empty
      val f4 = 't' :: 'h' :: 'i' :: Empty
      val f5 = 't' :: 'h' :: 'i' :: 's' :: Empty

      f1.viewR mustEqual None
      f2.viewR mustEqual Some((Empty, 't'))
      f3.viewR mustEqual Some((Single('t'), 'h'))
      f4.viewR mustEqual Some((Deep(One('t'), Empty, One('h')), 'i'))
      f5.viewR mustEqual Some((Deep(Two('t', 'h'), Empty, One('i')), 's'))
    }

    "support isEmpty method" in {
      val f1 = Empty
      val f2 = 't' :: Empty

      f1.isEmpty must beTrue
      f2.isEmpty must beFalse
    }

    "support headL method" in {
      val f1 = 't' :: Empty
      val f2 = 't' :: 'h' :: Empty
      val f3 = 't' :: 'h' :: 'i' :: Empty
      val f4 = 't' :: 'h' :: 'i' :: 's' :: Empty

      f1.headL mustEqual 't'
      f2.headL mustEqual 't'
      f3.headL mustEqual 't'
      f4.headL mustEqual 't'
    }

    "support headR method" in {
      val f1 = 't' :: Empty
      val f2 = 't' :: 'h' :: Empty
      val f3 = 't' :: 'h' :: 'i' :: Empty
      val f4 = 't' :: 'h' :: 'i' :: 's' :: Empty

      f1.headR mustEqual 't'
      f2.headR mustEqual 'h'
      f3.headR mustEqual 'i'
      f4.headR mustEqual 's'
    }

    "support tailL method" in {
      val f1 = 't' :: Empty
      val f2 = 't' :: 'h' :: Empty
      val f3 = 't' :: 'h' :: 'i' :: Empty
      val f4 = 't' :: 'h' :: 'i' :: 's' :: Empty

      f1.tailL mustEqual Empty
      f2.tailL mustEqual Single('h')
      f3.tailL mustEqual Deep(One('h'), Empty, One('i'))
      f4.tailL mustEqual Deep(Two('h', 'i'), Empty, One('s'))
    }

    "support tailR method" in {
      val f1 = 't' :: Empty
      val f2 = 't' :: 'h' :: Empty
      val f3 = 't' :: 'h' :: 'i' :: Empty
      val f4 = 't' :: 'h' :: 'i' :: 's' :: Empty

      f1.tailR mustEqual Empty
      f2.tailR mustEqual Single('t')
      f3.tailR mustEqual Deep(One('t'), Empty, One('h'))
      f4.tailR mustEqual Deep(Two('t', 'h'), Empty, One('i'))
    }

    "support concatenation to empty finger tree" in {
      val t1 = Empty
      val t2 = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty

      val t = t1 ++ t2
      t.toString mustEqual "thisisnotatree"
    }

    "support concatenation of empty finger tree" in {
      val t1 = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty
      val t2 = Empty

      val t = t1 ++ t2
      t.toString mustEqual "thisisnotatree"
    }

    "support concatenation to single element finger tree" in {
      val t1 = 't' :: Empty
      val t2 = 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty

      val t = t1 ++ t2
      t.toString mustEqual "thisisnotatree"
    }

    "support concatention to empty finger tree of single element finger tree" in {
      val t1 = Empty
      val t2 = 't' :: Empty

      val t = t1 ++ t2
      t.toString mustEqual "t"
    }

    "support concatenation to single element finger tree of empty finger tree" in {
      val t1 = 't' :: Empty
      val t2 = Empty

      val t = t1 ++ t2
      t.toString mustEqual "t"
    }

    "support concatenation of single element finger tree" in {
      val t1 = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: Empty
      val t2 = 'e' :: Empty

      val t = t1 ++ t2
      t.toString mustEqual "thisisnotatree"
    }

    "support concatenation of deep finger trees" in {
      val t1 = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: Empty
      val t2 = 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty

      val t = t1 ++ t2
      t.toString mustEqual "thisisnotatree"
    }

    "support concatenation on various splits of the string" in {
      def buildFingerTree(s: String) =
        s.foldRight[FingerTree[Char]](Empty) (_ :: _)

      val s = "thisisnotatree"
      (1 to s.length - 1).forall { i =>
        val s1 = s.substring(0, i)
        val s2 = s.substring(i)

        val t = buildFingerTree(s1) ++ buildFingerTree(s2)
        t.toString mustEqual "thisisnotatree"
      }
    }
  }
}
