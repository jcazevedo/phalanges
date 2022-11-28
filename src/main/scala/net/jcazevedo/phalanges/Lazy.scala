package net.jcazevedo.phalanges

import scala.annotation.tailrec

private[phalanges] sealed trait Lazy[+A] {
  lazy val value: A = Lazy.run(this)
  def map[B](f: A => B): Lazy[B] = flatMap(f.andThen(Lazy.pure))
  def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy.FlatMap(this, f)
}

private[phalanges] object Lazy {
  private[phalanges] def pure[A](x: A): Lazy[A] = Lazy.Pure(x)

  private[phalanges] def delay[A](x: => A): Lazy[A] = Lazy.Delay(() => x)

  private[phalanges] case class Pure[+A](x: A) extends Lazy[A]
  private[phalanges] case class Delay[+A](thunk: () => A) extends Lazy[A]
  private[phalanges] case class FlatMap[A, +B](orig: Lazy[A], f: A => Lazy[B]) extends Lazy[B]

  @tailrec
  private[phalanges] def run[A](lzy: Lazy[A]): A = {
    lzy match {
      case Pure(x) =>
        x

      case Delay(thunk) =>
        thunk()

      case FlatMap(x, f) =>
        x match {
          case Pure(x) =>
            run(f(x))

          case Delay(thunk) =>
            run(f(thunk()))

          case FlatMap(y, g) =>
            run(y.flatMap(a => g(a).flatMap(f)))
        }
    }
  }
}
