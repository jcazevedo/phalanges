package net.jcazevedo.phalange

import scala.collection.compat.immutable.LazyList

sealed abstract class FingerTree[V, A](implicit measured: Measured[A, V]) {
  private[phalange] def fold[B](
      empty: => B,
      single: A => B,
      deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
  ): B

  def measure: V =
    fold(empty = measured.empty, single = a => measured.apply(a), deep = (lm, _, _, _) => lm.value)

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
      single = b => FingerTree.deep(Digit(a), new Lazy(FingerTree.empty), Digit(b)),
      deep = (_, pr, m, sf) =>
        pr.fold(
          one = (_, b) => FingerTree.deep(Digit(a, b), m, sf),
          two = (_, b, c) => FingerTree.deep(Digit(a, b, c), m, sf),
          three = (_, b, c, d) => FingerTree.deep(Digit(a, b, c, d), m, sf),
          four = (_, b, c, d, e) => FingerTree.deep(Digit(a, b), new Lazy(Node(c, d, e) +: m.value), sf)
        )
    )

  def :+(a: A): FingerTree[V, A] =
    fold(
      empty = FingerTree.single(a),
      single = b => FingerTree.deep(Digit(b), new Lazy(FingerTree.empty), Digit(a)),
      deep = (_, pr, m, sf) =>
        sf.fold(
          one = (_, b) => FingerTree.deep(pr, m, Digit(b, a)),
          two = (_, c, b) => FingerTree.deep(pr, m, Digit(c, b, a)),
          three = (_, d, c, b) => FingerTree.deep(pr, m, Digit(d, c, b, a)),
          four = (_, e, d, c, b) => FingerTree.deep(pr, new Lazy(m.value :+ Node(e, d, c)), Digit(b, a))
        )
    )

  def ++(that: FingerTree[V, A]): FingerTree[V, A] =
    FingerTree.app3(this, List.empty, that)

  private def viewL: ViewL[V, A] =
    fold(
      empty = ViewL.Empty(),
      single = a => ViewL.Cons(a, new Lazy(FingerTree.empty)),
      deep = (_, pr, m, sf) =>
        pr.fold(
          one = (_, a) =>
            ViewL.Cons(
              a,
              new Lazy(m.value.viewL match {
                case ViewL.Empty() =>
                  FingerTree.measured(sf.toSeq: _*)

                case ViewL.Cons(node, rest) =>
                  node.fold(
                    node2 = (_, a, b) => FingerTree.deep(Digit(a, b), rest, sf),
                    node3 = (_, a, b, c) => FingerTree.deep(Digit(a, b, c), rest, sf)
                  )
              })
            ),
          two = (_, a, b) => ViewL.Cons(a, new Lazy(FingerTree.deep(Digit(b), m, sf))),
          three = (_, a, b, c) => ViewL.Cons(a, new Lazy(FingerTree.deep(Digit(b, c), m, sf))),
          four = (_, a, b, c, d) => ViewL.Cons(a, new Lazy(FingerTree.deep(Digit(b, c, d), m, sf)))
        )
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
      deep = (_, pr, m, sf) =>
        sf.fold(
          one = (_, a) =>
            ViewR.Cons(
              new Lazy(m.value.viewR match {
                case ViewR.Empty() =>
                  FingerTree.measured(pr.toSeq: _*)

                case ViewR.Cons(rest, node) =>
                  node.fold(
                    node2 = (_, b, a) => FingerTree.deep(pr, rest, Digit(b, a)),
                    node3 = (_, c, b, a) => FingerTree.deep(pr, rest, Digit(c, b, a))
                  )
              }),
              a
            ),
          two = (_, b, a) => ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit(b))), a),
          three = (_, c, b, a) => ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit(c, b))), a),
          four = (_, d, c, b, a) => ViewR.Cons(new Lazy(FingerTree.deep(pr, m, Digit(d, c, b))), a)
        )
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
          deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
      ): B =
        empty
    }

  private[phalange] def single[V, A](a: A)(implicit measured: Measured[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
      ): B =
        single(a)
    }

  private[phalange] def deep[V, A](pr: Digit[V, A], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[V, A])(implicit
      measured: Measured[A, V]
  ): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
      ): B =
        deep(
          new Lazy(measured.append(pr.measure, measured.append(m.value.measure, sf.measure))),
          pr,
          m,
          sf
        )
    }

  private[phalange] def nodes[V, A](a: A, b: A, rest: A*)(implicit measured: Measured[A, V]): List[Node[V, A]] =
    if (rest.isEmpty)
      List(Node(a, b))
    else if (rest.length == 1)
      List(Node(a, b, rest.head))
    else if (rest.length == 2)
      List(Node(a, b), Node(rest.head, rest.tail.head))
    else
      Node(a, b, rest.head) :: nodes(rest.tail.head, rest.tail.tail.head, rest.tail.tail.tail: _*)

  private[phalange] def app3[V, A](a: FingerTree[V, A], ts: List[A], c: FingerTree[V, A])(implicit
      measured: Measured[A, V]
  ): FingerTree[V, A] =
    a.fold(
      empty = ts.foldRight(c)(_ +: _),
      single = x => x +: (ts.foldRight(c)(_ +: _)),
      deep = (_, pr1, m1, sf1) =>
        c.fold(
          empty = ts.foldLeft(a)(_ :+ _),
          single = x => ts.foldLeft(a)(_ :+ _) :+ x,
          deep = (_, pr2, m2, sf2) =>
            sf1.fold(
              one = (_, a) =>
                if (ts.isEmpty)
                  pr2.fold(
                    one = (_, b) => FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b), m2.value)), sf2),
                    two = (_, b, c) => FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c), m2.value)), sf2),
                    three =
                      (_, b, c, d) => FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d), m2.value)), sf2),
                    four = (_, b, c, d, e) =>
                      FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, c, d, e), m2.value)), sf2)
                  )
                else
                  FingerTree
                    .deep(pr1, new Lazy(app3(m1.value, nodes(a, ts.head, (ts.tail ++ pr2.toSeq): _*), m2.value)), sf2),
              two = (_, a, b) =>
                FingerTree.deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (ts ++ pr2.toSeq): _*), m2.value)), sf2),
              three = (_, a, b, c) =>
                FingerTree
                  .deep(pr1, new Lazy(app3(m1.value, nodes(a, b, (c +: (ts ++ pr2.toSeq)): _*), m2.value)), sf2),
              four = (_, a, b, c, d) =>
                FingerTree.deep(
                  pr1,
                  new Lazy(app3(m1.value, nodes(a, b, (Seq(c, d) ++ ts ++ pr2.toSeq): _*), m2.value)),
                  sf2
                )
            )
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
