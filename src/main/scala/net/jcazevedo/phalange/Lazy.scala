package net.jcazevedo.phalange

import scala.annotation.tailrec

private[phalange] sealed trait Lazy[+A] {
  def run(): A = Lazy.run(this)
  def map[B](f: A => B): Lazy[B] = flatMap(f.andThen(Lazy.pure))
  def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy.FlatMap(this, f)
}

private[phalange] object Lazy {
  private[phalange] def pure[A](x: A): Lazy[A] = Lazy.Pure(x)

  private[phalange] def delay[A](x: => A): Lazy[A] = Lazy.Delay(() => x)

  private[phalange] case class Pure[+A](value: A) extends Lazy[A]
  private[phalange] case class Delay[+A](thunk: () => A) extends Lazy[A] {
    lazy val value = thunk()
  }
  private[phalange] case class FlatMap[A, +B](orig: Lazy[A], f: A => Lazy[B]) extends Lazy[B]

  @tailrec
  private[phalange] def run[A](lzy: Lazy[A]): A = {
    lzy match {
      case Pure(value) =>
        value

      case Delay(thunk) =>
        thunk()

      case FlatMap(x, f) =>
        x match {
          case Pure(value) =>
            run(f(value))

          case d: Delay[_] =>
            run(f(d.value))

          case FlatMap(y, g) =>
            run(y.flatMap(a => g(a).flatMap(f)))
        }
    }
  }
}
