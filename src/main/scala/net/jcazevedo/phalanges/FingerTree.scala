package net.jcazevedo.phalanges

import scala.collection.compat.immutable.LazyList

sealed abstract class FingerTree[V, A](implicit measured: Measured[A, V]) {
  private[phalanges] def fold[B](
      empty: => B,
      single: A => B,
      deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
  ): B

  lazy val measure: V =
    fold(empty = measured.empty, single = a => measured.apply(a), deep = (lm, _, _, _) => lm.value)

  def foldRight[B](z: B)(op: (A, B) => B): B =
    fold(
      empty = z,
      single = op(_, z),
      deep = (_, pr, m, sf) =>
        pr.foldRight(m.flatMap(x => Lazy.delay(x.foldRight(sf.foldRight(z)(op))((a, b) => a.foldRight(b)(op)))))(
          (a, b) => b.flatMap(x => Lazy.delay(op(a, x)))
        ).value
    )

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    fold(
      empty = z,
      single = op(z, _),
      deep = (_, pr, m, sf) =>
        sf.foldLeft(m.flatMap(x => Lazy.delay(x.foldLeft(pr.foldLeft(z)(op))((b, a) => a.foldLeft(b)(op)))))((b, a) =>
          b.flatMap(x => Lazy.delay(op(x, a)))
        ).value
    )

  def +:(a: A): FingerTree[V, A] =
    fold(
      empty = FingerTree.single(a),
      single = b => FingerTree.deep(Digit(a), Lazy.pure(FingerTree.empty), Digit(b)),
      deep = (_, pr, m, sf) =>
        pr.fold(
          one = (_, b) => FingerTree.deep(Digit(a, b), m, sf),
          two = (_, b, c) => FingerTree.deep(Digit(a, b, c), m, sf),
          three = (_, b, c, d) => FingerTree.deep(Digit(a, b, c, d), m, sf),
          four = (_, b, c, d, e) =>
            FingerTree.deep(Digit(a, b), for { node <- Lazy.pure(Node(c, d, e)); ft <- m } yield node +: ft, sf)
        )
    )

  def :+(a: A): FingerTree[V, A] =
    fold(
      empty = FingerTree.single(a),
      single = b => FingerTree.deep(Digit(b), Lazy.pure(FingerTree.empty), Digit(a)),
      deep = (_, pr, m, sf) =>
        sf.fold(
          one = (_, b) => FingerTree.deep(pr, m, Digit(b, a)),
          two = (_, c, b) => FingerTree.deep(pr, m, Digit(c, b, a)),
          three = (_, d, c, b) => FingerTree.deep(pr, m, Digit(d, c, b, a)),
          four = (_, e, d, c, b) =>
            FingerTree.deep(pr, for { node <- Lazy.pure(Node(e, d, c)); ft <- m } yield ft :+ node, Digit(b, a))
        )
    )

  def ++(that: FingerTree[V, A]): FingerTree[V, A] =
    FingerTree.app3(Lazy.pure(this), List.empty, Lazy.pure(that)).value

  private def viewL: ViewL[V, A] =
    fold(
      empty = ViewL.empty,
      single = a => ViewL.cons(a, Lazy.pure(FingerTree.empty)),
      deep = (_, pr, m, sf) =>
        pr.fold(
          one = (_, a) =>
            ViewL.cons(
              a,
              m.flatMap(
                _.viewL.fold(
                  empty = Lazy.delay(FingerTree.measured(sf.toSeq: _*)),
                  cons = (node, rest) =>
                    node.fold(
                      node2 = (_, a, b) => Lazy.delay(FingerTree.deep(Digit(a, b), rest, sf)),
                      node3 = (_, a, b, c) => Lazy.delay(FingerTree.deep(Digit(a, b, c), rest, sf))
                    )
                )
              )
            ),
          two = (_, a, b) => ViewL.cons(a, Lazy.delay(FingerTree.deep(Digit(b), m, sf))),
          three = (_, a, b, c) => ViewL.cons(a, Lazy.delay(FingerTree.deep(Digit(b, c), m, sf))),
          four = (_, a, b, c, d) => ViewL.cons(a, Lazy.delay(FingerTree.deep(Digit(b, c, d), m, sf)))
        )
    )

  def lazyListL: LazyList[A] =
    LazyList.unfold(this)(_.viewL.fold(empty = None, cons = (head, tail) => Some((head, tail.value))))

  def toList: List[A] =
    lazyListL.toList

  def isEmpty: Boolean =
    viewL.fold(empty = true, cons = (_, _) => false)

  def nonEmpty: Boolean =
    !isEmpty

  def headL: A =
    viewL.fold(empty = throw new NoSuchElementException("head of empty finger tree"), cons = (a, _) => a)

  def tailL: FingerTree[V, A] =
    viewL.fold(empty = throw new NoSuchElementException("tail of empty finger tree"), cons = (_, rest) => rest.value)

  def headLOption: Option[A] =
    viewL.fold(empty = None, cons = (a, _) => Some(a))

  def tailLOption: Option[FingerTree[V, A]] =
    viewL.fold(empty = None, cons = (_, rest) => Some(rest.value))

  private def viewR: ViewR[V, A] =
    fold(
      empty = ViewR.empty,
      single = a => ViewR.cons(Lazy.pure(FingerTree.empty), a),
      deep = (_, pr, m, sf) =>
        sf.fold(
          one = (_, a) =>
            ViewR.cons(
              m.flatMap(
                _.viewR.fold(
                  empty = Lazy.delay(FingerTree.measured(pr.toSeq: _*)),
                  cons = (rest, node) =>
                    node.fold(
                      node2 = (_, b, a) => Lazy.delay(FingerTree.deep(pr, rest, Digit(b, a))),
                      node3 = (_, c, b, a) => Lazy.delay(FingerTree.deep(pr, rest, Digit(c, b, a)))
                    )
                )
              ),
              a
            ),
          two = (_, b, a) => ViewR.cons(Lazy.delay(FingerTree.deep(pr, m, Digit(b))), a),
          three = (_, c, b, a) => ViewR.cons(Lazy.delay(FingerTree.deep(pr, m, Digit(c, b))), a),
          four = (_, d, c, b, a) => ViewR.cons(Lazy.delay(FingerTree.deep(pr, m, Digit(d, c, b))), a)
        )
    )

  def lazyListR: LazyList[A] =
    LazyList.unfold(this)(_.viewR.fold(empty = None, cons = (tail, head) => Some((head, tail.value))))

  def headR: A =
    viewR.fold(empty = throw new NoSuchElementException("head of empty finger tree"), cons = (_, a) => a)

  def tailR: FingerTree[V, A] =
    viewR.fold(empty = throw new NoSuchElementException("tail of empty finger tree"), cons = (rest, _) => rest.value)

  def headROption: Option[A] =
    viewR.fold(empty = None, cons = (_, a) => Some(a))

  def tailROption: Option[FingerTree[V, A]] =
    viewR.fold(empty = None, cons = (rest, _) => Some(rest.value))

  private[phalanges] def splitTree(p: V => Boolean, i: V): (Lazy[FingerTree[V, A]], A, Lazy[FingerTree[V, A]]) =
    fold(
      empty = throw new NoSuchElementException("splitTree of empty finger tree"),
      single = x => (Lazy.pure(FingerTree.empty), x, Lazy.pure(FingerTree.empty)),
      deep = (_, pr, m, sf) => {
        val vpr = measured.append(i, pr.measure)
        if (p(vpr)) {
          val (lzyL, x, lzyR) = pr.split(p, i)
          val l = lzyL.map(_.fold(FingerTree.empty)(digit => FingerTree.measured(digit.toSeq: _*)))
          val r = lzyR.map(nPr => FingerTree.deepL(nPr, m, sf))
          (l, x, r)
        } else {
          val vm = measured.append(vpr, m.value.measure)
          if (p(vm)) {
            val (lzyMl, xs, lzyMr) = m.value.splitTree(p, vpr)
            val (lzyL, x, lzyR) = xs.toDigit.split(p, measured.append(vpr, lzyMl.value.measure))
            val l = lzyL.map(nSf => FingerTree.deepR(pr, lzyMl, nSf))
            val r = lzyR.map(nPr => FingerTree.deepL(nPr, lzyMr, sf))
            (l, x, r)
          } else {
            val (lzyL, x, lzyR) = sf.split(p, vm)
            val l = lzyL.map(nSf => FingerTree.deepR(pr, m, nSf))
            val r = lzyR.map(_.fold(FingerTree.empty)(digit => FingerTree.measured(digit.toSeq: _*)))
            (l, x, r)
          }
        }
      }
    )

  def split(p: V => Boolean): (FingerTree[V, A], FingerTree[V, A]) =
    if (isEmpty) (FingerTree.empty, FingerTree.empty)
    else if (p(measure)) {
      val (l, x, r) = splitTree(p, measured.empty)
      (l.value, x +: r.value)
    } else (this, FingerTree.empty)

  def takeUntil(p: V => Boolean): FingerTree[V, A] =
    split(p)._1

  def dropUntil(p: V => Boolean): FingerTree[V, A] =
    split(p)._2
}

object FingerTree {
  private[phalanges] def empty[V, A](implicit measured: Measured[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
      ): B =
        empty
    }

  private[phalanges] def single[V, A](a: A)(implicit measured: Measured[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
      ): B =
        single(a)
    }

  private[phalanges] def deep[V, A](pr: Digit[V, A], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[V, A])(implicit
      measured: Measured[A, V]
  ): FingerTree[V, A] =
    new FingerTree[V, A] {
      def fold[B](
          empty: => B,
          single: A => B,
          deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
      ): B =
        deep(
          m.flatMap(ft => Lazy.delay(measured.append(pr.measure, measured.append(ft.measure, sf.measure)))),
          pr,
          m,
          sf
        )
    }

  private[phalanges] def deepL[V, A](prOpt: Option[Digit[V, A]], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[V, A])(
      implicit measured: Measured[A, V]
  ): FingerTree[V, A] =
    prOpt.fold(
      m.value.viewL
        .fold(empty = FingerTree.measured(sf.toSeq: _*), cons = (head, rest) => FingerTree.deep(head.toDigit, rest, sf))
    )(pr => FingerTree.deep(pr, m, sf))

  private[phalanges] def deepR[V, A](pr: Digit[V, A], m: Lazy[FingerTree[V, Node[V, A]]], sfOpt: Option[Digit[V, A]])(
      implicit measured: Measured[A, V]
  ): FingerTree[V, A] =
    sfOpt.fold(
      m.value.viewR
        .fold(empty = FingerTree.measured(pr.toSeq: _*), cons = (rest, head) => FingerTree.deep(pr, rest, head.toDigit))
    )(sf => FingerTree.deep(pr, m, sf))

  private[phalanges] def nodes[V, A](a: A, b: A, rest: A*)(implicit measured: Measured[A, V]): List[Node[V, A]] =
    if (rest.isEmpty)
      List(Node(a, b))
    else if (rest.length == 1)
      List(Node(a, b, rest.head))
    else if (rest.length == 2)
      List(Node(a, b), Node(rest.head, rest.tail.head))
    else
      Node(a, b, rest.head) :: nodes(rest.tail.head, rest.tail.tail.head, rest.tail.tail.tail: _*)

  private[phalanges] def app3[V, A](a: Lazy[FingerTree[V, A]], ts: List[A], c: Lazy[FingerTree[V, A]])(implicit
      measured: Measured[A, V]
  ): Lazy[FingerTree[V, A]] =
    a.flatMap(av =>
      av.fold(
        empty = c.map(ts.foldRight(_)(_ +: _)),
        single = x => c.map(cv => x +: (ts.foldRight(cv)(_ +: _))),
        deep = (_, pr1, m1, sf1) =>
          c.map(
            _.fold(
              empty = ts.foldLeft(av)(_ :+ _),
              single = x => ts.foldLeft(av)(_ :+ _) :+ x,
              deep = (_, pr2, m2, sf2) =>
                sf1.fold(
                  one = (_, a) =>
                    if (ts.isEmpty)
                      pr2.fold(
                        one = (_, b) => FingerTree.deep(pr1, app3(m1, nodes(a, b), m2), sf2),
                        two = (_, b, c) => FingerTree.deep(pr1, app3(m1, nodes(a, b, c), m2), sf2),
                        three = (_, b, c, d) => FingerTree.deep(pr1, app3(m1, nodes(a, b, c, d), m2), sf2),
                        four = (_, b, c, d, e) => FingerTree.deep(pr1, app3(m1, nodes(a, b, c, d, e), m2), sf2)
                      )
                    else
                      FingerTree.deep(pr1, app3(m1, nodes(a, ts.head, (ts.tail ++ pr2.toSeq): _*), m2), sf2),
                  two = (_, a, b) => FingerTree.deep(pr1, app3(m1, nodes(a, b, (ts ++ pr2.toSeq): _*), m2), sf2),
                  three =
                    (_, a, b, c) => FingerTree.deep(pr1, app3(m1, nodes(a, b, (c +: (ts ++ pr2.toSeq)): _*), m2), sf2),
                  four = (_, a, b, c, d) =>
                    FingerTree.deep(pr1, app3(m1, nodes(a, b, (Seq(c, d) ++ ts ++ pr2.toSeq): _*), m2), sf2)
                )
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
