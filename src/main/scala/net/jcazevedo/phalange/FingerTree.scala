package net.jcazevedo.phalange

import scala.collection.compat.immutable.LazyList

sealed trait FingerTree[V, +A] {
  def foldRight[B](z: B)(op: (A, B) => B): B =
    this match {
      case FingerTree.Empty() =>
        z

      case FingerTree.Single(x) =>
        op(x, z)

      case FingerTree.Deep(_, pr, m, sf) =>
        pr.foldRight(m.value.foldRight(sf.foldRight(z)(op))((a, b) => a.foldRight(b)(op)))(op)
    }

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    this match {
      case FingerTree.Empty() =>
        z

      case FingerTree.Single(x) =>
        op(z, x)

      case FingerTree.Deep(_, pr, m, sf) =>
        sf.foldLeft(m.value.foldLeft(pr.foldLeft(z)(op))((b, a) => a.foldLeft(b)(op)))(op)
    }

  def +:[B >: A](a: B)(implicit measured: Measured[B, V]): FingerTree[V, B] =
    this match {
      case FingerTree.Empty() =>
        FingerTree.Single(a)

      case FingerTree.Single(b) =>
        FingerTree.deep(Digit.One(a), new Lazy(FingerTree.Empty()), Digit.One(b))

      case FingerTree.Deep(_, Digit.One(b), m, sf) =>
        FingerTree.deep(Digit.Two(a, b), m, sf)

      case FingerTree.Deep(_, Digit.Two(b, c), m, sf) =>
        FingerTree.deep(Digit.Three(a, b, c), m, sf)

      case FingerTree.Deep(_, Digit.Three(b, c, d), m, sf) =>
        FingerTree.deep(Digit.Four(a, b, c, d), m, sf)

      case FingerTree.Deep(_, Digit.Four(b, c, d, e), m, sf) =>
        FingerTree.deep(Digit.Two(a, b), new Lazy(Node.node3(c, d, e) +: m.value), sf)
    }

  def :+[B >: A](a: B)(implicit measured: Measured[B, V]): FingerTree[V, B] =
    this match {
      case FingerTree.Empty() =>
        FingerTree.Single(a)

      case FingerTree.Single(b) =>
        FingerTree.deep(Digit.One(b), new Lazy(FingerTree.Empty()), Digit.One(a))

      case FingerTree.Deep(_, pr, m, Digit.One(b)) =>
        FingerTree.deep(pr, m, Digit.Two(b, a))

      case FingerTree.Deep(_, pr, m, Digit.Two(c, b)) =>
        FingerTree.deep(pr, m, Digit.Three(c, b, a))

      case FingerTree.Deep(_, pr, m, Digit.Three(d, c, b)) =>
        FingerTree.deep(pr, m, Digit.Four(d, c, b, a))

      case FingerTree.Deep(_, pr, m, Digit.Four(e, d, c, b)) =>
        FingerTree.deep(pr, new Lazy(m.value :+ Node.node3(e, d, c)), Digit.Two(b, a))
    }

  def ++[B >: A](that: FingerTree[V, B])(implicit measured: Measured[B, V]): FingerTree[V, B] =
    FingerTree.app3(this, List.empty, that)

  private def viewL(implicit measured: Measured[A, V]): ViewL[V, A] =
    this match {
      case FingerTree.Empty() =>
        ViewL.Empty()

      case FingerTree.Single(a) =>
        ViewL.Cons(a, new Lazy(FingerTree.empty))

      case FingerTree.Deep(_, Digit.One(a), m, sf) =>
        ViewL.Cons(
          a,
          new Lazy(m.value.viewL match {
            case ViewL.Empty() =>
              FingerTree.apply(sf.toSeq: _*)

            case ViewL.Cons(Node.Node2(_, a, b), rest) =>
              FingerTree.deep(Digit.Two(a, b), rest, sf)

            case ViewL.Cons(Node.Node3(_, a, b, c), rest) =>
              FingerTree.deep(Digit.Three(a, b, c), rest, sf)
          })
        )

      case FingerTree.Deep(_, Digit.Two(a, b), m, sf) =>
        ViewL.Cons(a, new Lazy(FingerTree.deep(Digit.One(b), m, sf)))

      case FingerTree.Deep(_, Digit.Three(a, b, c), m, sf) =>
        ViewL.Cons(a, new Lazy(FingerTree.deep(Digit.Two(b, c), m, sf)))

      case FingerTree.Deep(_, Digit.Four(a, b, c, d), m, sf) =>
        ViewL.Cons(a, new Lazy(FingerTree.deep(Digit.Three(b, c, d), m, sf)))
    }

  def lazyListL(implicit measured: Measured[A, V]): LazyList[A] =
    LazyList.unfold(this)(_.viewL match {
      case ViewL.Cons(head, tail) => Some((head, tail.value))
      case ViewL.Empty()          => None
    })

  def toList(implicit measured: Measured[A, V]): List[A] =
    lazyListL.toList

  def isEmpty(implicit measured: Measured[A, V]): Boolean =
    viewL match {
      case ViewL.Cons(_, _) => false
      case ViewL.Empty()    => true
    }

  def nonEmpty(implicit measured: Measured[A, V]): Boolean =
    !isEmpty

  def headL(implicit measured: Measured[A, V]): A =
    viewL match {
      case ViewL.Cons(a, _) => a
      case ViewL.Empty()    => throw new NoSuchElementException("head of empty finger tree")
    }

  def tailL(implicit measured: Measured[A, V]): FingerTree[V, A] =
    viewL match {
      case ViewL.Cons(_, rest) => rest.value
      case ViewL.Empty()       => throw new NoSuchElementException("tail of empty finger tree")
    }

  def headLOption(implicit measured: Measured[A, V]): Option[A] =
    viewL match {
      case ViewL.Cons(a, _) => Some(a)
      case ViewL.Empty()    => None
    }

  def tailLOption(implicit measured: Measured[A, V]): Option[FingerTree[V, A]] =
    viewL match {
      case ViewL.Cons(_, rest) => Some(rest.value)
      case ViewL.Empty()       => None
    }

  private def viewR(implicit measured: Measured[A, V]): ViewR[V, A] =
    this match {
      case FingerTree.Empty() =>
        ViewR.Empty()

      case FingerTree.Single(a) =>
        ViewR.Cons(new Lazy(FingerTree.empty), a)

      case FingerTree.Deep(_, pr, m, Digit.One(a)) =>
        ViewR.Cons(
          new Lazy(m.value.viewR match {
            case ViewR.Empty() =>
              FingerTree.apply(pr.toSeq: _*)

            case ViewR.Cons(rest, Node.Node2(_, b, a)) =>
              FingerTree.deep(pr, rest, Digit.Two(b, a))

            case ViewR.Cons(rest, Node.Node3(_, c, b, a)) =>
              FingerTree.deep(pr, rest, Digit.Three(c, b, a))
          }),
          a
        )

      case FingerTree.Deep(_, pr, m, Digit.Two(b, a)) =>
        ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit.One(b))), a)

      case FingerTree.Deep(_, pr, m, Digit.Three(c, b, a)) =>
        ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit.Two(c, b))), a)

      case FingerTree.Deep(_, pr, m, Digit.Four(d, c, b, a)) =>
        ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit.Three(d, c, b))), a)
    }

  def lazyListR(implicit measured: Measured[A, V]): LazyList[A] =
    LazyList.unfold(this)(_.viewR match {
      case ViewR.Cons(tail, head) => Some((head, tail.value))
      case ViewR.Empty()          => None
    })

  def headR(implicit measured: Measured[A, V]): A =
    viewR match {
      case ViewR.Cons(_, a) => a
      case ViewR.Empty()    => throw new NoSuchElementException("head of empty finger tree")
    }

  def tailR(implicit measured: Measured[A, V]): FingerTree[V, A] =
    viewR match {
      case ViewR.Cons(rest, _) => rest.value
      case ViewR.Empty()       => throw new NoSuchElementException("tail of empty finger tree")
    }

  def headROption(implicit measured: Measured[A, V]): Option[A] =
    viewR match {
      case ViewR.Cons(_, a) => Some(a)
      case ViewR.Empty()    => None
    }

  def tailROption(implicit measured: Measured[A, V]): Option[FingerTree[V, A]] =
    viewR match {
      case ViewR.Cons(rest, _) => Some(rest.value)
      case ViewR.Empty()       => None
    }
}

object FingerTree {
  private[phalange] case class Empty[V]() extends FingerTree[V, Nothing]
  private[phalange] case class Single[V, +A](x: A) extends FingerTree[V, A]
  private[phalange] case class Deep[V, A](v: Lazy[V], pr: Digit[A], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[A])
      extends FingerTree[V, A]

  private[phalange] def deep[V, A](pr: Digit[A], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[A])(implicit measured: Measured[A, V]): FingerTree[V, A] =
    FingerTree.Deep(
      new Lazy(measured.append(measured.append(Measured.measure[Digit[A], V](pr), Measured.measure[FingerTree[V, Node[V, A]], V](m.value)), Measured.measure[Digit[A], V](sf))),
      pr,
      m,
      sf
    )

  private[phalange] def nodes[V, A](a: A, b: A, rest: A*)(implicit measured: Measured[A, V]): List[Node[V, A]] =
    if (rest.isEmpty)
      List(Node.node2(a, b))
    else if (rest.length == 1)
      List(Node.node3(a, b, rest.head))
    else if (rest.length == 2)
      List(Node.node2(a, b), Node.node2(rest.head, rest.tail.head))
    else
      Node.node3(a, b, rest.head) :: nodes(rest.tail.head, rest.tail.tail.head, rest.tail.tail.tail: _*)

  private[phalange] def app3[V, A](a: FingerTree[V, A], b: List[A], c: FingerTree[V, A])(implicit measured: Measured[A, V]): FingerTree[V, A] =
    (a, b, c) match {
      case (FingerTree.Empty(), ts, xs) =>
        ts.foldRight(xs)(_ +: _)

      case (xs, ts, FingerTree.Empty()) =>
        ts.foldLeft(xs)(_ :+ _)

      case (FingerTree.Single(x), ts, xs) =>
        x +: (ts.foldRight(xs)(_ +: _))

      case (xs, ts, FingerTree.Single(x)) =>
        (ts.foldLeft(xs)(_ :+ _)) :+ x

      case (FingerTree.Deep(_, pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(_, Digit.One(b), m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(_, Digit.Two(b, c), m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(_, Digit.Three(b, c, d), m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.One(a)), Nil, FingerTree.Deep(_, Digit.Four(b, c, d, e), m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d, e), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.One(a)), ts, FingerTree.Deep(_, pr2, m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, ts.head, (ts.tail ++ pr2.toSeq): _*), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.Two(a, b)), ts, FingerTree.Deep(_, pr2, m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (ts ++ pr2.toSeq): _*), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.Three(a, b, c)), ts, FingerTree.Deep(_, pr2, m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (c +: (ts ++ pr2.toSeq)): _*), m2.value)), sf2)

      case (FingerTree.Deep(_, pr1, m1, Digit.Four(a, b, c, d)), ts, FingerTree.Deep(_, pr2, m2, sf2)) =>
        FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (Seq(c, d) ++ ts ++ pr2.toSeq): _*), m2.value)), sf2)
    }

  def empty[V, A]: FingerTree[V, A] =
    Empty()

  def apply[V, A](as: A*)(implicit measured: Measured[A, V]): FingerTree[V, A] =
    as.foldRight(FingerTree.empty[V, A])(_ +: _)
}
