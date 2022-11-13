package net.jcazevedo.phalange

private[phalange] class Lazy[+T](x: => T) {
  lazy val value = x
}
