package net.jcazevedo.phalange

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
  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
  def ::(a: A): FingerTree[A]
  def +(a: A): FingerTree[A]
  def viewL: Option[(A, FingerTree[A])]
  def viewR: Option[(FingerTree[A], A)]
  def toList = foldRight (List[A]()) (_ :: _)
  override def toString = toList mkString ""

  def isEmpty = viewL match {
    case None => true
    case Some((_, _)) => false
  }

  def headL = (viewL: @unchecked) match {
    case Some((h, _)) => h
  }

  def headR = (viewR: @unchecked) match {
    case Some((_, h)) => h
  }

  def tailL = (viewL: @unchecked) match {
    case Some((_, t)) => t
  }

  def tailR = (viewR: @unchecked) match {
    case Some((t, _)) => t
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
  def viewR = None
}

case class Single[A](x: A) extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(x, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, x)
  def ::(a: A): FingerTree[A] = Deep(Digit(a), Empty(), Digit(x))
  def +(a: A): FingerTree[A] = Deep(Digit(x), Empty(), Digit(a))
  def viewL = Some(x, Empty[A]())
  def viewR = Some(Empty[A](), x)
}

case class Deep[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
     extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    def f1(d: Digit[A], b: B) = (d foldRight(b)) (f(_, _))
    def f2(t: FingerTree[Node[A]], b: B): B =
      (t foldRight(b)) ((a, b) => (a foldRight(b)) (f(_, _)))

    f1(pr, f2(m, f1(sf, z)))
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def f1(b: B, d: Digit[A]) = (d foldLeft(b)) (f(_, _))
    def f2(b: B, t: FingerTree[Node[A]]): B =
      (t foldLeft(b)) ((b, a) => (a foldLeft(b)) (f(_, _)))

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

  def viewL = Some((pr.headL, deepL(pr.tailL, m, sf)))
  def viewR = Some((deepR(pr, m, sf.tailR), sf.headR))

  def deepL(pr: Option[Digit[A]], m: FingerTree[Node[A]], sf: Digit[A]) = {
    pr match {
      case Some(d) => Deep(d, m, sf)
      case None => m.viewL match {
        case None => sf.toTree
        case Some((a, m)) => Deep(a.toDigit, m, sf)
      }
    }
  }

  def deepR(pr: Digit[A], m: FingerTree[Node[A]], sf: Option[Digit[A]]) = {
    sf match {
      case Some(d) => Deep(pr, m, d)
      case None => m.viewR match {
        case None => pr.toTree
        case Some((m, a)) => Deep(pr, m, a.toDigit)
      }
    }
  }
}

trait Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
  def headL: A
  def tailL: Option[Digit[A]]
  def headR: A
  def tailR: Option[Digit[A]]
  def toTree: FingerTree[A]
}

case class One[A](a: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, a)
  def headL = a
  def tailL = None
  def headR = a
  def tailR = None
  def toTree = a :: Empty()
}

case class Two[A](a: A, b: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, z))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(z, a), b)
  def headL = a
  def tailL = Some(Digit(b))
  def headR = b
  def tailR = Some(Digit(a))
  def toTree = a :: b :: Empty()
}

case class Three[A](a: A, b: A, c: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, f(c, z)))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(f(z, a), b), c)
  def headL = a
  def tailL = Some(Digit(b, c))
  def headR = c
  def tailR = Some(Digit(a, b))
  def toTree = a :: b :: c :: Empty()
}

case class Four[A](a: A, b: A, c: A, d: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, f(c, f(d, z))))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(f(f(z, a), b), c), d)
  def headL = a
  def tailL = Some(Digit(b, c, d))
  def headR = d
  def tailR = Some(Digit(a, b, c))
  def toTree = a :: b :: c :: d :: Empty()
}

object Digit {
  def apply[A](a: A): Digit[A] = One(a)
  def apply[A](a: A, b: A): Digit[A] = Two(a, b)
  def apply[A](a: A, b: A, c: A): Digit[A] = Three(a, b, c)
  def apply[A](a: A, b: A, c: A, d: A): Digit[A] = Four(a, b, c, d)
}
