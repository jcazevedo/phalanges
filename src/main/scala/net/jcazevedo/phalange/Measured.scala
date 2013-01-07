package net.jcazevedo.phalange

trait Measured[A, V] {
  def apply(a: A): V
}
