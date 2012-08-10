import scala.collection.mutable.ListBuffer

trait Tree[A]
case class Zero[A](a: A) extends Tree[A]
case class Succ[A](a: Tree[Node[A]]) extends Tree[A]

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
}

case class Empty[A] extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = z
  def foldLeft[B](z: B)(f: (B, A) => B): B = z
}

case class Single[A](x: A) extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = f(x, z)
  def foldLeft[B](z: B)(f: (B, A) => B): B = f(z, x)
}

case class Deep[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
     extends FingerTree[A] {
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    def f1(d: Digit[A], b: B) = {
      d.a.foldRight(b) {
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
      d.a.foldLeft(b) {
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
}

class Digit[A](val a: ListBuffer[A])

object Digit {
  def apply[A](v: A*) = new Digit(ListBuffer(v:_*))
}

object Main extends App {
  val t = Succ(Zero(Node(Succ(Zero(Node(Succ(Zero(Node('t', 'h'))),
                                        Succ(Zero(Node('i', 's'))),
                                        Succ(Zero(Node('i', 's')))))),
                         Succ(Zero(Node(Succ(Zero(Node('n', 'o', 't'))),
                                        Succ(Zero(Node('a', 't'))),
                                        Succ(Zero(Node('r', 'e', 'e')))))))))

  val f: FingerTree[Char] = Deep(Digit('t', 'h'),
                                 Deep(Digit(Node('i', 's'), Node('i', 's')),
                                      Empty(),
                                      Digit(Node('n', 'o', 't'), Node('a', 't'))),
                                 Digit('r', 'e', 'e'))
}
