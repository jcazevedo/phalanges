import scala.collection.mutable.ListBuffer

trait Tree[A]
case class Zero[A](a: A) extends Tree[A]
case class Succ[A](a: Tree[Node[A]]) extends Tree[A]

trait Node[A]
case class Node2[A](a1: A, a2: A) extends Node[A]
case class Node3[A](a1: A, a2: A, a3: A) extends Node[A]

object Node {
  def apply[A](a1: A, a2: A): Node[A] = Node2(a1, a2)
  def apply[A](a1: A, a2: A, a3: A): Node[A] = Node3(a1, a2, a3)
}

trait FingerTree[-A]
case class Empty[A] extends FingerTree[A]
case class Single[A](o: A) extends FingerTree[A]
case class Deep[A](dl: Digit[A], t: FingerTree[Node[A]], dr: Digit[A]) extends FingerTree[A]

class Digit[+A](a: ListBuffer[A])

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

  val f = Deep(Digit('i', 'h'),
               Deep(Digit(Node('i', 's'), Node('i', 's')),
                    Empty[Node[Any]],
                    Digit(Node('n', 'o', 't'), Node('a', 't'))),
               Digit('r', 'e', 'e'))
}
