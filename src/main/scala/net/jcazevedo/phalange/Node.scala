package net.jcazevedo.phalange

trait Node[+A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C
  def toDigit: Digit[A]
}

case class Node2[+A](a: A, b: A) extends Node[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C) = f(a, f(b, z))
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C) = f(f(z, a), b)
  def toDigit = Two(a, b)
}

case class Node3[+A](a: A, b: A, c: A) extends Node[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C) = f(a, f(b, f(c, z)))
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C) = f(f(f(z, a), b), c)
  def toDigit = Three(a, b, c)
}

object Node {
  def apply[A](a: A, b: A): Node[A] = Node2(a, b)
  def apply[A](a: A, b: A, c: A): Node[A] = Node3(a, b, c)
  def nodes[A](s: List[A]): List[Node[A]] =
    s match {
      case a :: b :: Nil => Node(a, b) :: Nil
      case a :: b :: c :: Nil => Node(a, b, c) :: Nil
      case a :: b :: c :: d :: Nil => Node(a, b) :: Node(c, d) :: Nil
      case a :: b :: c :: xs => Node(a, b, c) :: nodes(xs)
      case _ => Nil
    }
}
