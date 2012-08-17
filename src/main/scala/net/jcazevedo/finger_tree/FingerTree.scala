package net.jcazevedo.finger_tree

trait Node[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
  def toDigit: Digit[A]
}

case class Node2[A](a: A, b: A) extends Node[A] {
  def foldRight[B](z: B)(f: (A, B) => B) = f(a, f(b, z))
  def foldLeft[B](z: B)(f: (B, A) => B) = f(f(z, a), b)
  def toDigit = Two(a, b)
}

case class Node3[A](a: A, b: A, c: A) extends Node[A] {
  def foldRight[B](z: B)(f: (A, B) => B) = f(a, f(b, f(c, z)))
  def foldLeft[B](z: B)(f: (B, A) => B) = f(f(f(z, a), b), c)
  def toDigit = Three(a, b, c)
}

object Node {
  def apply[A](a: A, b: A): Node[A] = Node2(a, b)
  def apply[A](a: A, b: A, c: A): Node[A] = Node3(a, b, c)
}

trait FingerTree[A] {
  type LeftView = Option[(A, FingerTree[A])]

  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
  def ::(a: A): FingerTree[A]
  def +(a: A): FingerTree[A]
  def viewL: LeftView

  def toList = foldRight(List[A]()) { (h, l) =>
    h :: l
  }
  def isEmpty = viewL match {
    case None => true
    case Some((_, _)) => false
  }
  def headL = (viewL: @unchecked) match {
    case Some((h, _)) => h
  }
  def tailL = (viewL: @unchecked) match {
    case Some((_, t)) => t
  }
}

object FingerTree {
  def toTree[A](s: Traversable[A]) = {
    s.foldRight[FingerTree[A]](Empty[A]()) { (h, t) =>
      h :: t
    }
  }
}

case class Empty[A]() extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = z
  def foldLeft[B](z: B)(f: (B, A) => B): B = z
  def ::(a: A): FingerTree[A] = Single(a)
  def +(a: A): FingerTree[A] = Single(a)
  def viewL = None
}

case class Single[A](x: A) extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(x, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, x)
  def ::(a: A): FingerTree[A] = Deep(Digit(a), Empty(), Digit(x))
  def +(a: A): FingerTree[A] = Deep(Digit(x), Empty(), Digit(a))
  def viewL = Some(x, Empty[A]())
}

case class Deep[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
     extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    def f1(d: Digit[A], b: B) = {
      d.foldRight(b) {
        (a, b) => f(a, b)
      }
    }

    def f2(t: FingerTree[Node[A]], b: B) = {
      t.foldRight(b) {
        (a, b) => a.foldRight(b) {
          (a, b) => f(a, b)
        }
      }
    }

    f1(pr, f2(m, f1(sf, z)))
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def f1(b: B, d: Digit[A]) = {
      d.foldLeft(b) {
        (b, a) => f(b, a)
      }
    }

    def f2(b: B, t: FingerTree[Node[A]]) = {
      t.foldLeft(b) {
        (b, a) => a.foldLeft(b) {
          (b, a) => f(b, a)
        }
      }
    }

    f1(f2(f1(z, pr), m), sf)
  }

  def ::(a: A): FingerTree[A] = {
    pr match {
      case Four(b, c, d, e) => Deep(Digit(a, b), (Node(c, d, e) :: m), sf)
      case Three(b, c, d) => Deep(Digit(a, b, c, d), m, sf)
      case Two(b, c) => Deep(Digit(a, b, c), m, sf)
      case One(b) => Deep(Digit(a, b), m, sf)
    }
  }

  def +(a: A): FingerTree[A] = {
    sf match {
      case Four(e, d, c, b) => Deep(pr, m + Node(e, d, c), Digit(b, a))
      case Three(d, c, b) => Deep(pr, m, Digit(d, c, b, a))
      case Two(c, b) => Deep(pr, m, Digit(c, b, a))
      case One(b) => Deep(pr, m, Digit(b, a))
    }
  }

  def viewL = Some((pr.head, deepL(pr.tail, m, sf)))

  def deepL(pr: Option[Digit[A]], m: FingerTree[Node[A]], sf: Digit[A]) = {
    pr match {
      case Some(d) => Deep(d, m, sf)
      case None => m.viewL match {
        case None => sf.toTree
        case Some((a, m)) => Deep(a.toDigit, m, sf)
      }
    }
  }
}

trait Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
  def head: A
  def tail: Option[Digit[A]]
  def toTree: FingerTree[A]
}

case class One[A](a: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, a)
  def head = a
  def tail = None
  def toTree = a :: Empty()
}

case class Two[A](a: A, b: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, z))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(z, a), b)
  def head = a
  def tail = Some(Digit(b))
  def toTree = a :: b :: Empty()
}

case class Three[A](a: A, b: A, c: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, f(c, z)))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(f(z, a), b), c)
  def head = a
  def tail = Some(Digit(b, c))
  def toTree = a :: b :: c :: Empty()
}

case class Four[A](a: A, b: A, c: A, d: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, f(c, f(d, z))))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(f(f(z, a), b), c), d)
  def head = a
  def tail = Some(Digit(b, c, d))
  def toTree = a :: b :: c :: d :: Empty()
}

object Digit {
  def apply[A](a: A): Digit[A] = One(a)
  def apply[A](a: A, b: A): Digit[A] = Two(a, b)
  def apply[A](a: A, b: A, c: A): Digit[A] = Three(a, b, c)
  def apply[A](a: A, b: A, c: A, d: A): Digit[A] = Four(a, b, c, d)
}
