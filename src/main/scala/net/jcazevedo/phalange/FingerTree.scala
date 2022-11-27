package net.jcazevedo.phalange

import scala.collection.compat.immutable.LazyList

sealed abstract class FingerTree[V, A](implicit measured: Measured[A, V]) {
  private[phalange] def fold[B](
      empty: => B,
      single: A => B,
      deep: (Lazy[V], Digit[V, A], Lazy[FingerTree[V, Node[V, A]]], Digit[V, A]) => B
  ): B

  def measure: V =
    fold(empty = measured.empty, single = a => measured.apply(a), deep = (lm, _, _, _) => lm.run())

  def foldRight[B](z: B)(op: (A, B) => B): B =
    fold(
      empty = z,
      single = op(_, z),
      deep = (_, pr, m, sf) => pr.foldRight(m.run().foldRight(sf.foldRight(z)(op))((a, b) => a.foldRight(b)(op)))(op)
    )

  def foldLeft[B](z: B)(op: (B, A) => B): B =
    fold(
      empty = z,
      single = op(z, _),
      deep = (_, pr, m, sf) => sf.foldLeft(m.run().foldLeft(pr.foldLeft(z)(op))((b, a) => a.foldLeft(b)(op)))(op)
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
          four = (_, b, c, d, e) => FingerTree.deep(Digit(a, b), m.flatMap(ft => Lazy.delay(Node(c, d, e) +: ft)), sf)
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
          four = (_, e, d, c, b) => FingerTree.deep(pr, m.flatMap(ft => Lazy.delay(ft :+ Node(e, d, c))), Digit(b, a))
        )
    )

  def ++(that: FingerTree[V, A]): FingerTree[V, A] =
    FingerTree.app3(this, List.empty, that)

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
    LazyList.unfold(this)(_.viewL.fold(empty = None, cons = (head, tail) => Some((head, tail.run()))))

  def toList: List[A] =
    lazyListL.toList

  def isEmpty: Boolean =
    viewL.fold(empty = true, cons = (_, _) => false)

  def nonEmpty: Boolean =
    !isEmpty

  def headL: A =
    viewL.fold(empty = throw new NoSuchElementException("head of empty finger tree"), cons = (a, _) => a)

  def tailL: FingerTree[V, A] =
    viewL.fold(empty = throw new NoSuchElementException("tail of empty finger tree"), cons = (_, rest) => rest.run())

  def headLOption: Option[A] =
    viewL.fold(empty = None, cons = (a, _) => Some(a))

  def tailLOption: Option[FingerTree[V, A]] =
    viewL.fold(empty = None, cons = (_, rest) => Some(rest.run()))

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
    LazyList.unfold(this)(_.viewR.fold(empty = None, cons = (tail, head) => Some((head, tail.run()))))

  def headR: A =
    viewR.fold(empty = throw new NoSuchElementException("head of empty finger tree"), cons = (_, a) => a)

  def tailR: FingerTree[V, A] =
    viewR.fold(empty = throw new NoSuchElementException("tail of empty finger tree"), cons = (rest, _) => rest.run())

  def headROption: Option[A] =
    viewR.fold(empty = None, cons = (_, a) => Some(a))

  def tailROption: Option[FingerTree[V, A]] =
    viewR.fold(empty = None, cons = (rest, _) => Some(rest.run()))

  private[phalange] def splitTree(p: V => Boolean, i: V): (FingerTree[V, A], A, FingerTree[V, A]) =
    fold(
      empty = throw new NoSuchElementException("splitTree of empty finger tree"),
      single = x => (FingerTree.empty, x, FingerTree.empty),
      deep = (_, pr, m, sf) => {
        val vpr = measured.append(i, pr.measure)
        if (p(vpr)) {
          val (l, x, r) = pr.split(p, i)
          (l.fold(FingerTree.empty)(digit => FingerTree.measured(digit.toSeq: _*)), x, FingerTree.deepL(r, m, sf))
        } else {
          val vm = measured.append(vpr, m.run().measure)
          if (p(vm)) {
            val (ml, xs, mr) = m.run().splitTree(p, vpr)
            val (l, x, r) = xs.toDigit.split(p, measured.append(vpr, ml.measure))
            (FingerTree.deepR(pr, Lazy.pure(ml), l), x, FingerTree.deepL(r, Lazy.pure(mr), sf))
          } else {
            val (l, x, r) = sf.split(p, vm)
            (FingerTree.deepR(pr, m, l), x, r.fold(FingerTree.empty)(digit => FingerTree.measured(digit.toSeq: _*)))
          }
        }
      }
    )

  def split(p: V => Boolean): (FingerTree[V, A], FingerTree[V, A]) =
    if (isEmpty) (FingerTree.empty, FingerTree.empty)
    else if (p(measure)) {
      val (l, x, r) = splitTree(p, measured.empty)
      (l, x +: r)
    } else (this, FingerTree.empty)

  def takeUntil(p: V => Boolean): FingerTree[V, A] =
    split(p)._1

  def dropUntil(p: V => Boolean): FingerTree[V, A] =
    split(p)._2
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
          m.flatMap(ft => Lazy.delay(measured.append(pr.measure, measured.append(ft.measure, sf.measure)))),
          pr,
          m,
          sf
        )
    }

  private[phalange] def deepL[V, A](prOpt: Option[Digit[V, A]], m: Lazy[FingerTree[V, Node[V, A]]], sf: Digit[V, A])(
      implicit measured: Measured[A, V]
  ): FingerTree[V, A] =
    prOpt.fold(
      m.run()
        .viewL
        .fold(empty = FingerTree.measured(sf.toSeq: _*), cons = (head, rest) => FingerTree.deep(head.toDigit, rest, sf))
    )(pr => FingerTree.deep(pr, m, sf))

  private[phalange] def deepR[V, A](pr: Digit[V, A], m: Lazy[FingerTree[V, Node[V, A]]], sfOpt: Option[Digit[V, A]])(
      implicit measured: Measured[A, V]
  ): FingerTree[V, A] =
    sfOpt.fold(
      m.run()
        .viewR
        .fold(empty = FingerTree.measured(pr.toSeq: _*), cons = (rest, head) => FingerTree.deep(pr, rest, head.toDigit))
    )(sf => FingerTree.deep(pr, m, sf))

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
                    one = (_, b) =>
                      FingerTree
                        .deep(pr1, m1.flatMap(m1V => m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b), m2V)))), sf2),
                    two = (_, b, c) =>
                      FingerTree.deep(
                        pr1,
                        m1.flatMap(m1V => m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b, c), m2V)))),
                        sf2
                      ),
                    three = (_, b, c, d) =>
                      FingerTree.deep(
                        pr1,
                        m1.flatMap(m1V => m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b, c, d), m2V)))),
                        sf2
                      ),
                    four = (_, b, c, d, e) =>
                      FingerTree.deep(
                        pr1,
                        m1.flatMap(m1V => m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b, c, d, e), m2V)))),
                        sf2
                      )
                  )
                else
                  FingerTree
                    .deep(
                      pr1,
                      m1.flatMap(m1V =>
                        m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, ts.head, (ts.tail ++ pr2.toSeq): _*), m2V)))
                      ),
                      sf2
                    ),
              two = (_, a, b) =>
                FingerTree.deep(
                  pr1,
                  m1.flatMap(m1V => m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b, (ts ++ pr2.toSeq): _*), m2V)))),
                  sf2
                ),
              three = (_, a, b, c) =>
                FingerTree
                  .deep(
                    pr1,
                    m1.flatMap(m1V =>
                      m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b, (c +: (ts ++ pr2.toSeq)): _*), m2V)))
                    ),
                    sf2
                  ),
              four = (_, a, b, c, d) =>
                FingerTree.deep(
                  pr1,
                  m1.flatMap(m1V =>
                    m2.flatMap(m2V => Lazy.delay(app3(m1V, nodes(a, b, (Seq(c, d) ++ ts ++ pr2.toSeq): _*), m2V)))
                  ),
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
