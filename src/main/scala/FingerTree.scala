trait Tree[A]
case class Zero[A](a: A) extends Tree[A]
case class Succ[A](a: Tree[Node[A]]) extends Tree[A]

trait Node[A]
case class Node2[A](a1: A, a2: A) extends Node[A]
case class Node3[A](a1: A, a2: A, a3: A) extends Node[A]

object Node {
  def apply[A](a1: A, a2: A) = Node2(a1, a2)
  def apply[A](a1: A, a2: A, a3: A) = Node3(a1, a2, a3)
}

object Main extends App {
  val t = Succ(Zero(Node(Succ(Zero(Node(Succ(Zero(Node('t', 'h'))),
                                        Succ(Zero(Node('i', 's'))),
                                        Succ(Zero(Node('i', 's')))))),
                         Succ(Zero(Node(Succ(Zero(Node('n', 'o', 't')))
                                        Succ(Zero(Node('a', 't'))),
                                        Succ(Zero(Node('r', 'e', 'e')))))))))
}
