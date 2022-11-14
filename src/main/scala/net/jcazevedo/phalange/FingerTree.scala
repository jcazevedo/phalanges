package net.jcazevedo.phalange

import scala.collection.compat.immutable.LazyList

sealed abstract class FingerTree[V, A](implicit measured: Measured[A, V]) {
  private[phalange] def fold[B](
      empty: => B,
      single: A => B,
      deep: (Lazy[V], Digit[A], Lazy[FingerTree[V, Node[V, A]]], Digit[A]) => B
  ): B

  def foldRight[B](z: B)(op: (A, B) => B): B =
    fold(
      empty = z,
      single = op(_, z),
      deep = (_, pr, m, sf) => pr.foldRight(m.value.foldRight(sf.foldRight(z)(op))((a, b) => a.foldRight(b)(op)))(op)
    )

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    fold(
      empty = z,
      single = op(z, _),
      deep = (_, pr, m, sf) => sf.foldLeft(m.value.foldLeft(pr.foldLeft(z)(op))((b, a) => a.foldLeft(b)(op)))(op)
    )

  def +:(a: A): FingerTree[V, A] =
    fold(
      empty = FingerTree.single(a),
      single = b => FingerTree.deep(Digit.One(a), new Lazy(FingerTree.empty), Digit.One(b)),
      deep = {
        case (_, Digit.One(b), m, sf) =>
          FingerTree.deep(Digit.Two(a, b), m, sf)

        case (_, Digit.Two(b, c), m, sf) =>
          FingerTree.deep(Digit.Three(a, b, c), m, sf)

        case (_, Digit.Three(b, c, d), m, sf) =>
          FingerTree.deep(Digit.Four(a, b, c, d), m, sf)

        case (_, Digit.Four(b, c, d, e), m, sf) =>
          FingerTree.deep(Digit.Two(a, b), new Lazy(Node.node3(c, d, e) +: m.value), sf)
      }
    )

  def :+(a: A): FingerTree[V, A] =
    fold(
      empty = FingerTree.single(a),
      single = b => FingerTree.deep(Digit.One(b), new Lazy(FingerTree.empty), Digit.One(a)),
      deep = {
        case (_, pr, m, Digit.One(b)) =>
          FingerTree.deep(pr, m, Digit.Two(b, a))

        case (_, pr, m, Digit.Two(c, b)) =>
          FingerTree.deep(pr, m, Digit.Three(c, b, a))

        case (_, pr, m, Digit.Three(d, c, b)) =>
          FingerTree.deep(pr, m, Digit.Four(d, c, b, a))

        case (_, pr, m, Digit.Four(e, d, c, b)) =>
          FingerTree.deep(pr, new Lazy(m.value :+ Node.node3(e, d, c)), Digit.Two(b, a))
      }
    )

  def ++(that: FingerTree[V, A]): FingerTree[V, A] =
    FingerTree.app3(this, List.empty, that)

  private def viewL: ViewL[V, A] =
    fold(
      empty = ViewL.Empty(),
      single = a => ViewL.Cons(a, new Lazy(FingerTree.empty)),
      deep = {
        case (_, Digit.One(a), m, sf) =>
          ViewL.Cons(
            a,
            new Lazy(m.value.viewL match {
              case ViewL.Empty() =>
                FingerTree.measured(sf.toSeq: _*)

              case ViewL.Cons(Node.Node2(_, a, b), rest) =>
                FingerTree.deep(Digit.Two(a, b), rest, sf)

              case ViewL.Cons(Node.Node3(_, a, b, c), rest) =>
                FingerTree.deep(Digit.Three(a, b, c), rest, sf)
            })
          )

        case (_, Digit.Two(a, b), m, sf) =>
          ViewL.Cons(a, new Lazy(FingerTree.deep(Digit.One(b), m, sf)))

        case (_, Digit.Three(a, b, c), m, sf) =>
          ViewL.Cons(a, new Lazy(FingerTree.deep(Digit.Two(b, c), m, sf)))

        case (_, Digit.Four(a, b, c, d), m, sf) =>
          ViewL.Cons(a, new Lazy(FingerTree.deep(Digit.Three(b, c, d), m, sf)))
      }
    )

  def lazyListL: LazyList[A] =
    LazyList.unfold(this)(_.viewL match {
      case ViewL.Cons(head, tail) => Some((head, tail.value))
      case ViewL.Empty()          => None
    })

  def toList: List[A] =
    lazyListL.toList

  def isEmpty: Boolean =
    viewL match {
      case ViewL.Cons(_, _) => false
      case ViewL.Empty()    => true
    }

  def nonEmpty: Boolean =
    !isEmpty

  def headL: A =
    viewL match {
      case ViewL.Cons(a, _) => a
      case ViewL.Empty()    => throw new NoSuchElementException("head of empty finger tree")
    }

  def tailL: FingerTree[V, A] =
    viewL match {
      case ViewL.Cons(_, rest) => rest.value
      case ViewL.Empty()       => throw new NoSuchElementException("tail of empty finger tree")
    }

  def headLOption: Option[A] =
    viewL match {
      case ViewL.Cons(a, _) => Some(a)
      case ViewL.Empty()    => None
    }

  def tailLOption: Option[FingerTree[V, A]] =
    viewL match {
      case ViewL.Cons(_, rest) => Some(rest.value)
      case ViewL.Empty()       => None
    }

  private def viewR: ViewR[V, A] =
    fold(
      empty = ViewR.Empty(),
      single = a => ViewR.Cons(new Lazy(FingerTree.empty), a),
      deep = {
        case (_, pr, m, Digit.One(a)) =>
          ViewR.Cons(
            new Lazy(m.value.viewR match {
              case ViewR.Empty() =>
                FingerTree.measured(pr.toSeq: _*)

              case ViewR.Cons(rest, Node.Node2(_, b, a)) =>
                FingerTree.deep(pr, rest, Digit.Two(b, a))

              case ViewR.Cons(rest, Node.Node3(_, c, b, a)) =>
                FingerTree.deep(pr, rest, Digit.Three(c, b, a))
            }),
            a
          )

        case (_, pr, m, Digit.Two(b, a)) =>
          ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit.One(b))), a)

        case (_, pr, m, Digit.Three(c, b, a)) =>
          ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit.Two(c, b))), a)

        case (_, pr, m, Digit.Four(d, c, b, a)) =>
          ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit.Three(d, c, b))), a)
      }
    )

  def lazyListR: LazyList[A] =
    LazyList.unfold(this)(_.viewR match {
      case ViewR.Cons(tail, head) => Some((head, tail.value))
      case ViewR.Empty()          => None
    })

  def headR: A =
    viewR match {
      case ViewR.Cons(_, a) => a
      case ViewR.Empty()    => throw new NoSuchElementException("head of empty finger tree")
    }

  def tailR: FingerTree[V, A] =
    viewR match {
      case ViewR.Cons(rest, _) => rest.value
      case ViewR.Empty()       => throw new NoSuchElementException("tail of empty finger tree")
    }

  def headROption: Option[A] =
    viewR match {
      case ViewR.Cons(_, a) => Some(a)
      case ViewR.Empty()    => None
    }

  def tailROption: Option[FingerTree[V, A]] =
    viewR match {
      case ViewR.Cons(rest, _) => Some(rest.value)
      case ViewR.Empty()       => None
    }
}

object FingerTree {
  private[phalange] def empty[V, A](implicit measured: Measured[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[A], Lazy[FingerTree[V, Node[V, A]]], Digit[A]) => B
      ): B =
        empty
    }

  private[phalange] def single[V, A](a: A)(implicit measured: Measured[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[A], Lazy[FingerTree[V, Node[V, A]]], Digit[A]) => B
      ): B =
        single(a)
    }

  private[phalange] def deep[V, A](pr: Digit[A], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[A])(implicit
      measured: Measured[A, V]
  ): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[A], Lazy[FingerTree[V, Node[V, A]]], Digit[A]) => B
      ): B =
        deep(
          new Lazy(
            measured.append(measured.append(Measured.measure(pr), Measured.measure(m.value)), Measured.measure(sf))
          ),
          pr,
          m,
          sf
        )
    }

  private[phalange] def nodes[V, A](a: A, b: A, rest: A*)(implicit measured: Measured[A, V]): List[Node[V, A]] =
    if (rest.isEmpty)
      List(Node.node2(a, b))
    else if (rest.length == 1)
      List(Node.node3(a, b, rest.head))
    else if (rest.length == 2)
      List(Node.node2(a, b), Node.node2(rest.head, rest.tail.head))
    else
      Node.node3(a, b, rest.head) :: nodes(rest.tail.head, rest.tail.tail.head, rest.tail.tail.tail: _*)

  private[phalange] def app3[V, A](a: FingerTree[V, A], b: List[A], c: FingerTree[V, A])(implicit
      measured: Measured[A, V]
  ): FingerTree[V, A] =
    a.fold(
      empty = b.foldRight(c)(_ +: _),
      single = x => x +: (b.foldRight(c)(_ +: _)),
      deep = (_, pr1, m1, sf1) =>
        c.fold(
          empty = b.foldLeft(a)(_ :+ _),
          single = x => b.foldLeft(a)(_ :+ _) :+ x,
          deep = (_, pr2, m2, sf2) =>
            (pr1, m1, sf1, b, pr2, m2, sf2) match {
              case (pr1, m1, Digit.One(a), Nil, Digit.One(b), m2, sf2) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b), m2.value)), sf2)

              case (pr1, m1, Digit.One(a), Nil, Digit.Two(b, c), m2, sf2) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c), m2.value)), sf2)

              case (pr1, m1, Digit.One(a), Nil, Digit.Three(b, c, d), m2, sf2) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d), m2.value)), sf2)

              case (pr1, m1, Digit.One(a), Nil, Digit.Four(b, c, d, e), m2, sf2) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d, e), m2.value)), sf2)

              case (pr1, m1, Digit.One(a), ts, pr2, m2, sf2) =>
                FingerTree.deep(
                  pr1,
                  new Lazy(app3(m1.value, nodes(a, ts.head, (ts.tail ++ pr2.toSeq): _*), m2.value)),
                  sf2
                )

              case (pr1, m1, Digit.Two(a, b), ts, pr2, m2, sf2) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (ts ++ pr2.toSeq): _*), m2.value)), sf2)

              case (pr1, m1, Digit.Three(a, b, c), ts, pr2, m2, sf2) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (c +: (ts ++ pr2.toSeq)): _*), m2.value)), sf2)

              case (pr1, m1, Digit.Four(a, b, c, d), ts, pr2, m2, sf2) =>
                FingerTree.deep(
                  pr1,
                  new Lazy(app3(m1.value, nodes(a, b, (Seq(c, d) ++ ts ++ pr2.toSeq): _*), m2.value)),
                  sf2
                )
            }
        )
    )

  def measured[V, A](as: A*)(implicit measured: Measured[A, V]): FingerTree[V, A] =
    as.foldRight(FingerTree.empty[V, A])(_ +: _)

  def apply[A](as: A*): FingerTree[Unit, A] = {
    implicit val unitMeasured: Measured[A, Unit] =
      new Measured[A, Unit] {
        def apply(a: A): Unit = ()
        def empty: Unit = ()
        def append(a: Unit, b: Unit) = ()
      }

    as.foldRight(FingerTree.empty[Unit, A])(_ +: _)
  }
}
