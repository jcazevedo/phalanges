package net.jcazevedo.phalange

import scala.language.implicitConversions

trait FingerTree[+A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C
  def ::[B >: A](a: B): FingerTree[B]
  def +[B >: A](a: B): FingerTree[B]
  def ++[B >: A](xs: FingerTree[B], ts: List[B] = List()): FingerTree[B]
  def viewL: Option[(A, FingerTree[A])]
  def viewR: Option[(FingerTree[A], A)]
  def toList = foldRight(List[A]())(_ :: _)
  override def toString = toList mkString ""

  def isEmpty = viewL match {
    case None         => true
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
  object Implicits {
    implicit def toTree[A](s: Traversable[A]): FingerTree[A] = {
      s.foldRight[FingerTree[A]](Empty) { (h, t) =>
        h :: t
      }
    }
  }
}

case object Empty extends FingerTree[Nothing] {
  def foldRight[B >: Nothing, C](z: C)(f: (B, C) => C): C = z
  def foldLeft[B >: Nothing, C](z: C)(f: (C, B) => C): C = z
  def ::[A](a: A): FingerTree[A] = Single(a)
  def +[A](a: A): FingerTree[A] = Single(a)
  def ++[A](xs: FingerTree[A], ts: List[A]): FingerTree[A] =
    (ts foldRight (xs))(_ :: _)
  def viewL = None
  def viewR = None
}

case class Single[+A](x: A) extends FingerTree[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C = f(x, z)
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C = f(z, x)
  def ::[B >: A](a: B): FingerTree[B] = Deep(Digit(a), Empty, Digit(x))
  def +[B >: A](a: B): FingerTree[B] = Deep(Digit(x), Empty, Digit(a))
  def ++[B >: A](xs: FingerTree[B], ts: List[B]) =
    xs match {
      case Empty        => (ts.foldLeft[FingerTree[B]](this))(_ + _)
      case s: Single[_] => (ts.foldLeft[FingerTree[B]](this))(_ + _) + s.x
      case _            => x :: (ts foldRight (xs)) (_ :: _)
    }
  def viewL = Some((x, Empty))
  def viewR = Some((Empty, x))
}

class Deep[+A](val pr: Digit[A], val m: Lazy[FingerTree[Node[A]]], val sf: Digit[A]) extends FingerTree[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C = {
    def f1(d: Digit[B], b: C) = (d foldRight (b))(f(_, _))
    def f2(t: FingerTree[Node[B]], b: C): C =
      (t foldRight (b))((a, b) => (a foldRight (b))(f(_, _)))

    f1(pr, f2(m.t, f1(sf, z)))
  }

  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C = {
    def f1(b: C, d: Digit[B]) = (d foldLeft (b))(f(_, _))
    def f2(b: C, t: FingerTree[Node[B]]): C =
      (t foldLeft (b))((b, a) => (a foldLeft (b))(f(_, _)))

    f1(f2(f1(z, pr), m.t), sf)
  }

  def ::[B >: A](a: B): FingerTree[B] = {
    pr match {
      case Four(b, c, d, e) => Deep(Digit(a, b), (Node(c, d, e) :: m.t), sf)
      case Three(b, c, d)   => Deep(Digit(a, b, c, d), m, sf)
      case Two(b, c)        => Deep(Digit(a, b, c), m, sf)
      case One(b)           => Deep(Digit(a, b), m, sf)
    }
  }

  def +[B >: A](a: B): FingerTree[B] = {
    sf match {
      case Four(e, d, c, b) => Deep(pr, m.t + Node(e, d, c), Digit(b, a))
      case Three(d, c, b)   => Deep(pr, m, Digit(d, c, b, a))
      case Two(c, b)        => Deep(pr, m, Digit(c, b, a))
      case One(b)           => Deep(pr, m, Digit(b, a))
    }
  }

  def ++[B >: A](xs: FingerTree[B], ts: List[B]): FingerTree[B] =
    xs match {
      case Empty              => (ts.foldLeft[FingerTree[B]](this))(_ + _)
      case s: Single[_]       => (ts.foldLeft[FingerTree[B]](this))(_ + _) + s.x
      case Deep(pr2, m2, sf2) => Deep(pr, m.t ++ (m2, Node.nodes(sf.toList ++ ts ++ pr2.toList)), sf2)
    }

  def viewL = Some((pr.headL, deepL(pr.tailL, m, sf)))
  def viewR = Some((deepR(pr, m, sf.tailR), sf.headR))

  def deepL[B >: A](pr: Option[Digit[B]], m: Lazy[FingerTree[Node[B]]], sf: Digit[B]): FingerTree[B] = {
    pr match {
      case Some(d) => Deep(d, m, sf)
      case None =>
        m.t.viewL match {
          case None         => sf.toTree
          case Some((a, m)) => Deep(a.toDigit, m, sf)
        }
    }
  }

  def deepR[B >: A](pr: Digit[B], m: Lazy[FingerTree[Node[B]]], sf: Option[Digit[B]]): FingerTree[B] = {
    sf match {
      case Some(d) => Deep(pr, m, d)
      case None =>
        m.t.viewR match {
          case None         => pr.toTree
          case Some((m, a)) => Deep(pr, m, a.toDigit)
        }
    }
  }

  override def equals(that: Any): Boolean =
    that match {
      case Deep(tpr, tm, tsf) => pr == tpr && m.t == tm && sf == tsf
      case _                  => false
    }
}

object Deep {
  def apply[A](pr: Digit[A], m: => FingerTree[Node[A]], sf: Digit[A]) =
    new Deep(pr, new Lazy(m), sf)

  def apply[A](pr: Digit[A], m: Lazy[FingerTree[Node[A]]], sf: Digit[A]) =
    new Deep(pr, m, sf)

  def unapply[A](deep: Deep[A]): Option[(Digit[A], FingerTree[Node[A]], Digit[A])] =
    Some((deep.pr, deep.m.t, deep.sf))
}
