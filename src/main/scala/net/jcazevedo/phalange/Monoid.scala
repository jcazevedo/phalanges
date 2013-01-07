package net.jcazevedo.phalange

trait Monoid[A] {
  def append(a1: A, a2: A): A
  def empty: A
}
