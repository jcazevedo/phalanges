package net.jcazevedo.phalange

class Lazy[+T](t0: => T) {
  lazy val t = t0
}
