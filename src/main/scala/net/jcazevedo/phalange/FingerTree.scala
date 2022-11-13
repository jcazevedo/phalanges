package net.jcazevedo.phalange

import scala.collection.compat.immutable.LazyList

sealed trait FingerTree[+A] {
  def foldRight[B](z: B)(op: (A, B) => B): B =
    this match {
      case FingerTree.Empty =>
        z

      case FingerTree.Single(x) =>
        op(x, z)

      case FingerTree.Deep(pr, m, sf) =>
        pr.foldRight(m.value.foldRight(sf.foldRight(z)(op))((a, b) => a.foldRight(b)(op)))(op)
    }

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    this match {
      case FingerTree.Empty =>
        z

      case FingerTree.Single(x) =>
        op(z, x)

      case FingerTree.Deep(pr, m, sf) =>
        sf.foldLeft(m.value.foldLeft(pr.foldLeft(z)(op))((b, a) => a.foldLeft(b)(op)))(op)
    }

  def +:[B >: A](a: B): FingerTree[B] =
    this match {
      case FingerTree.Empty =>
        FingerTree.Single(a)

      case FingerTree.Single(b) =>
        FingerTree.Deep(Digit.One(a), new Lazy(FingerTree.Empty), Digit.One(b))

      case FingerTree.Deep(Digit.One(b), m, sf) =>
        FingerTree.Deep(Digit.Two(a, b), m, sf)

      case FingerTree.Deep(Digit.Two(b, c), m, sf) =>
        FingerTree.Deep(Digit.Three(a, b, c), m, sf)

      case FingerTree.Deep(Digit.Three(b, c, d), m, sf) =>
        FingerTree.Deep(Digit.Four(a, b, c, d), m, sf)

      case FingerTree.Deep(Digit.Four(b, c, d, e), m, sf) =>
        FingerTree.Deep(Digit.Two(a, b), new Lazy(Node.Node3(c, d, e) +: m.value), sf)
    }

  def :+[B >: A](a: B): FingerTree[B] =
    this match {
      case FingerTree.Empty =>
        FingerTree.Single(a)

      case FingerTree.Single(b) =>
        FingerTree.Deep(Digit.One(b), new Lazy(FingerTree.Empty), Digit.One(a))

      case FingerTree.Deep(pr, m, Digit.One(b)) =>
        FingerTree.Deep(pr, m, Digit.Two(b, a))

      case FingerTree.Deep(pr, m, Digit.Two(c, b)) =>
        FingerTree.Deep(pr, m, Digit.Three(c, b, a))

      case FingerTree.Deep(pr, m, Digit.Three(d, c, b)) =>
        FingerTree.Deep(pr, m, Digit.Four(d, c, b, a))

      case FingerTree.Deep(pr, m, Digit.Four(e, d, c, b)) =>
        FingerTree.Deep(pr, new Lazy(m.value :+ Node.Node3(e, d, c)), Digit.Two(b, a))
    }

  def ++[B >: A](that: FingerTree[B]): FingerTree[B] =
    FingerTree.app3(this, List.empty, that)

  private def viewL: ViewL[A] =
    this match {
      case FingerTree.Empty =>
        ViewL.Empty

      case FingerTree.Single(a) =>
        ViewL.Cons(a, new Lazy(FingerTree.empty))

      case FingerTree.Deep(Digit.One(a), m, sf) =>
        ViewL.Cons(
          a,
          new Lazy(m.value.viewL match {
            case ViewL.Empty =>
              FingerTree.apply(sf.toSeq: _*)

            case ViewL.Cons(Node.Node2(a, b), rest) =>
              FingerTree.Deep(Digit.Two(a, b), rest, sf)

            case ViewL.Cons(Node.Node3(a, b, c), rest) =>
              FingerTree.Deep(Digit.Three(a, b, c), rest, sf)
          })
        )

      case FingerTree.Deep(Digit.Two(a, b), m, sf) =>
        ViewL.Cons(a, new Lazy(FingerTree.Deep(Digit.One(b), m, sf)))

      case FingerTree.Deep(Digit.Three(a, b, c), m, sf) =>
        ViewL.Cons(a, new Lazy(FingerTree.Deep(Digit.Two(b, c), m, sf)))

      case FingerTree.Deep(Digit.Four(a, b, c, d), m, sf) =>
        ViewL.Cons(a, new Lazy(FingerTree.Deep(Digit.Three(b, c, d), m, sf)))
    }

  def lazyListL: LazyList[A] =
    LazyList.unfold(this)(_.viewL match {
      case ViewL.Cons(head, tail) => Some((head, tail.value))
      case ViewL.Empty            => None
    })

  def toList: List[A] =
    lazyListL.toList

  def isEmpty: Boolean =
    viewL match {
      case ViewL.Cons(_, _) => false
      case ViewL.Empty      => true
    }

  def nonEmpty: Boolean =
    !isEmpty

  def headL: A =
    viewL match {
      case ViewL.Cons(a, _) => a
      case ViewL.Empty      => throw new NoSuchElementException("head of empty finger tree")
    }

  def tailL: FingerTree[A] =
    viewL match {
      case ViewL.Cons(_, rest) => rest.value
      case ViewL.Empty         => throw new NoSuchElementException("tail of empty finger tree")
    }

  def headLOption: Option[A] =
    viewL match {
      case ViewL.Cons(a, _) => Some(a)
      case ViewL.Empty      => None
    }

  def tailLOption: Option[FingerTree[A]] =
    viewL match {
      case ViewL.Cons(_, rest) => Some(rest.value)
      case ViewL.Empty         => None
    }

  private def viewR: ViewR[A] =
    this match {
      case FingerTree.Empty =>
        ViewR.Empty

      case FingerTree.Single(a) =>
        ViewR.Cons(new Lazy(FingerTree.empty), a)

      case FingerTree.Deep(pr, m, Digit.One(a)) =>
        ViewR.Cons(
          new Lazy(m.value.viewR match {
            case ViewR.Empty =>
              FingerTree.apply(pr.toSeq: _*)

            case ViewR.Cons(rest, Node.Node2(b, a)) =>
              FingerTree.Deep(pr, rest, Digit.Two(b, a))

            case ViewR.Cons(rest, Node.Node3(c, b, a)) =>
              FingerTree.Deep(pr, rest, Digit.Three(c, b, a))
          }),
          a
        )

      case FingerTree.Deep(pr, m, Digit.Two(b, a)) =>
        ViewR.Cons(new Lazy(FingerTree.Deep(pr, m, Digit.One(b))), a)

      case FingerTree.Deep(pr, m, Digit.Three(c, b, a)) =>
        ViewR.Cons(new Lazy(FingerTree.Deep(pr, m, Digit.Two(c, b))), a)

      case FingerTree.Deep(pr, m, Digit.Four(d, c, b, a)) =>
        ViewR.Cons(new Lazy(FingerTree.Deep(pr, m, Digit.Three(d, c, b))), a)
    }

  def lazyListR: LazyList[A] =
    LazyList.unfold(this)(_.viewR match {
      case ViewR.Cons(tail, head) => Some((head, tail.value))
      case ViewR.Empty            => None
    })

  def headR: A =
    viewR match {
      case ViewR.Cons(_, a) => a
      case ViewR.Empty      => throw new NoSuchElementException("head of empty finger tree")
    }

  def tailR: FingerTree[A] =
    viewR match {
      case ViewR.Cons(rest, _) => rest.value
      case ViewR.Empty         => throw new NoSuchElementException("tail of empty finger tree")
    }

  def headROption: Option[A] =
    viewR match {
      case ViewR.Cons(_, a) => Some(a)
      case ViewR.Empty      => None
    }

  def tailROption: Option[FingerTree[A]] =
    viewR match {
      case ViewR.Cons(rest, _) => Some(rest.value)
      case ViewR.Empty         => None
    }
}

object FingerTree {
  private[phalange] case object Empty extends FingerTree[Nothing]
  private[phalange] case class Single[+A](x: A) extends FingerTree[A]
  private[phalange] case class Deep[A](pr: Digit[A], m: Lazy[FingerTree[Node[A]]], sf: Digit[A]) extends FingerTree[A]

  private[phalange] def nodes[A](a: A, b: A, rest: A*): List[Node[A]] =
    if (rest.isEmpty)
      List(Node.Node2(a, b))
    else if (rest.length == 1)
      List(Node.Node3(a, b, rest.head))
    else if (rest.length == 2)
      List(Node.Node2(a, b), Node.Node2(rest.head, rest.tail.head))
    else
      Node.Node3(a, b, rest.head) :: nodes(rest.tail.head, rest.tail.tail.head, rest.tail.tail.tail: _*)

  private[phalange] def app3[A](a: FingerTree[A], b: List[A], c: FingerTree[A]): FingerTree[A] =
    (a, b, c) match {
      case (FingerTree.Empty, ts, xs) =>
        ts.foldRight(xs)(_ +: _)

      case (xs, ts, FingerTree.Empty) =>
        ts.foldLeft(xs)(_ :+ _)

      case (FingerTree.Single(x), ts, xs) =>
        x +: (ts.foldRight(xs)(_ +: _))

      case (xs, ts, FingerTree.Single(x)) =>
        (ts.foldLeft(xs)(_ :+ _)) :+ x

      case (FingerTree.Deep(pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(Digit.One(b), m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(Digit.Two(b, c), m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(Digit.Three(b, c, d), m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(Digit.Four(b, c, d, e), m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d, e), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.One(a)), ts, FingerTree.Deep(pr2, m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, ts.head, (ts.tail ++ pr2.toSeq): _*), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.Two(a, b)), ts, FingerTree.Deep(pr2, m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (ts ++ pr2.toSeq): _*), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.Three(a, b, c)), ts, FingerTree.Deep(pr2, m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (c +: (ts ++ pr2.toSeq)): _*), m2.value)), sf2)

      case (FingerTree.Deep(pr1, m1, Digit.Four(a, b, c, d)), ts, FingerTree.Deep(pr2, m2, sf2)) =>
        FingerTree.Deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (Seq(c, d) ++ ts ++ pr2.toSeq): _*), m2.value)), sf2)
    }

  def empty[A]: FingerTree[A] =
    Empty

  def apply[A](as: A*): FingerTree[A] =
    as.foldRight(FingerTree.empty[A])(_ +: _)
}
