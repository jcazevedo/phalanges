package net.jcazevedo.phalange

trait Digit[+A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C
  def headL: A
  def tailL: Option[Digit[A]]
  def headR: A
  def tailR: Option[Digit[A]]
  def toTree: FingerTree[A]
  def toList = foldRight(List[A]()) (_ :: _)
}

case class One[+A](a: A) extends Digit[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C = f(a, z)
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C = f(z, a)
  def headL = a
  def tailL = None
  def headR = a
  def tailR = None
  def toTree = a :: Empty
}

case class Two[+A](a: A, b: A) extends Digit[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C = f(a, f(b, z))
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C = f(f(z, a), b)
  def headL = a
  def tailL = Some(Digit(b))
  def headR = b
  def tailR = Some(Digit(a))
  def toTree = a :: b :: Empty
}

case class Three[+A](a: A, b: A, c: A) extends Digit[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C = f(a, f(b, f(c, z)))
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C = f(f(f(z, a), b), c)
  def headL = a
  def tailL = Some(Digit(b, c))
  def headR = c
  def tailR = Some(Digit(a, b))
  def toTree = a :: b :: c :: Empty
}

case class Four[+A](a: A, b: A, c: A, d: A) extends Digit[A] {
  def foldRight[B >: A, C](z: C)(f: (B, C) => C): C = f(a, f(b, f(c, f(d, z))))
  def foldLeft[B >: A, C](z: C)(f: (C, B) => C): C = f(f(f(f(z, a), b), c), d)
  def headL = a
  def tailL = Some(Digit(b, c, d))
  def headR = d
  def tailR = Some(Digit(a, b, c))
  def toTree = a :: b :: c :: d :: Empty
}

object Digit {
  def apply[A](a: A): Digit[A] = One(a)
  def apply[A](a: A, b: A): Digit[A] = Two(a, b)
  def apply[A](a: A, b: A, c: A): Digit[A] = Three(a, b, c)
  def apply[A](a: A, b: A, c: A, d: A): Digit[A] = Four(a, b, c, d)
}
