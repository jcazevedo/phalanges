package net.jcazevedo.phalanges

object Main extends App {
  val length = 100000

  implicit val sizeMeasured: Measured[Int, Int] =
    new Measured[Int, Int] {
      def apply(v: Int): Int = 1
      def empty: Int = 0
      def append(a: Int, b: Int): Int = a + b
    }

  val ft = FingerTree.measured((1 to length): _*)
  val list = List.apply((1 to length): _*)

  val start1 = System.currentTimeMillis()
  // while (true) {
  println(ft.dropUntil(_ > 10000).headL)
  // }
  val dt1 = System.currentTimeMillis() - start1
  println(dt1)

  // val l1 = (0 until length).foldRight[MyListCC[Int]](MyNil)(_ +: _)
  // val l2 = (0 until length).foldRight(MyListFunc.empty[Int])(_ +: _)

  // println(l2.last)
  // println(l2)

  // val v1 = Lazy.delay({ println("a"); 1 }).flatMap(a => { println("b"); Lazy.delay({ println("c"); a + 2 }) })
  // v1.run()
  // v1.run()

  // println("---")

  // val v2 = Eval.later({ println("a"); 1 }).flatMap(a => { println("b"); Eval.later({ println("c"); a + 2 }) })
  // v2.value
  // v2.value
}
