package net.jcazevedo.phalanges

trait Monoid[A] {
  def empty: A
  def append(a: A, b: A): A
}
