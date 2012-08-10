import scala.collection.mutable.ListBuffer

trait Node[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
}

case class Node2[A](a: A, b: A) extends Node[A] {
  def foldRight[B](z: B)(f: (A, B) => B) = f(a, f(b, z))
  def foldLeft[B](z: B)(f: (B, A) => B) = f(f(z, a), b)
}

case class Node3[A](a: A, b: A, c: A) extends Node[A] {
  def foldRight[B](z: B)(f: (A, B) => B) = f(a, f(b, f(c, z)))
  def foldLeft[B](z: B)(f: (B, A) => B) = f(f(f(z, a), b), c)
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
}

case class Empty[A] extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = z
  def foldLeft[B](z: B)(f: (B, A) => B): B = z
  def ::(a: A): FingerTree[A] = Single(a)
  def +(a: A): FingerTree[A] = Single(a)
}

case class Single[A](x: A) extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(x, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, x)
  def ::(a: A): FingerTree[A] = Deep(Digit(a), Empty(), Digit(x))
  def +(a: A): FingerTree[A] = Deep(Digit(x), Empty(), Digit(a))
}

case class Deep[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
     extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    def f1(d: Digit[A], b: B) = {
      d.foldRight(b) {
        (a, b) => f(a, b)
      }
    }

    def f2(t: FingerTree[Node[A]], b: B) = {
      t.foldRight(b) {
        (a, b) => a.foldRight(b) {
          (a, b) => f(a, b)
        }
      }
    }

    f1(pr, f2(m, f1(sf, z)))
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def f1(b: B, d: Digit[A]) = {
      d.foldLeft(b) {
        (b, a) => f(b, a)
      }
    }

    def f2(b: B, t: FingerTree[Node[A]]) = {
      t.foldLeft(b) {
        (b, a) => a.foldLeft(b) {
          (b, a) => f(b, a)
        }
      }
    }

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
}

trait Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B
  def foldLeft[B](z: B)(f: (B, A) => B): B
}

case class One[A](a: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, a)
}

case class Two[A](a: A, b: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, z))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(z, a), b)
}

case class Three[A](a: A, b: A, c: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, f(c, z)))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(f(z, a), b), c)
}

case class Four[A](a: A, b: A, c: A, d: A) extends Digit[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(a, f(b, f(c, f(d, z))))
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(f(f(f(z, a), b), c), d)
}

object Digit {
  def apply[A](a: A): Digit[A] = One(a)
  def apply[A](a: A, b: A): Digit[A] = Two(a, b)
  def apply[A](a: A, b: A, c: A): Digit[A] = Three(a, b, c)
  def apply[A](a: A, b: A, c: A, d: A): Digit[A] = Four(a, b, c, d)
}

object Main extends App {
  val f1: FingerTree[Char] = Deep(Digit('t', 'h'),
                                 Deep(Digit(Node('i', 's'), Node('i', 's')),
                                      Empty(),
                                      Digit(Node('n', 'o', 't'), Node('a', 't'))),
                                 Digit('r', 'e', 'e'))

  val f2a: FingerTree[Char] = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: Empty()
  val f2: FingerTree[Char] = f2a + 'a' + 't' + 'r' + 'e' + 'e'
  println (f2.foldRight("") { (c, s) =>
    c + s
  })
}
