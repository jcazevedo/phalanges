import net.jcazevedo.finger_tree._
import org.specs2.mutable._
import org.specs2.specification.AfterExample

class FingerTreeSpec extends Specification {
  "A Finger Tree" should {
    "support fold right operation" in {
      val f: FingerTree[Char] = Deep(Digit('t', 'h'),
                                      Deep(Digit(Node('i', 's'), Node('i', 's')),
                                           Empty(),
                                           Digit(Node('n', 'o', 't'), Node('a', 't'))),
                                      Digit('r', 'e', 'e'))

      val l = f.foldRight(List[Char]()) { (c, l) =>
        c :: l
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support fold left operation" in {
      val f: FingerTree[Char] = Deep(Digit('t', 'h'),
                                      Deep(Digit(Node('i', 's'), Node('i', 's')),
                                           Empty(),
                                           Digit(Node('n', 'o', 't'), Node('a', 't'))),
                                      Digit('r', 'e', 'e'))

      val l = f.foldLeft(List[Char]()) { (l, c) =>
        l ++ List(c)
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support cons operation" in {
      val f: FingerTree[Char] = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: 'o' :: 't' :: 'a' :: 't' :: 'r' :: 'e' :: 'e' :: Empty()

      val l = f.foldRight(List[Char]()) { (c, l) =>
        c :: l
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support snoc operation" in {
      val f: FingerTree[Char] = Empty() + 't' + 'h' + 'i' + 's' + 'i' + 's' + 'n' + 'o' + 't' + 'a' + 't' + 'r' + 'e' + 'e'

      val l = f.foldRight(List[Char]()) { (c, l) =>
        c :: l
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }

    "support cons and snoc operations intertwined" in {
      val f: FingerTree[Char] = 't' :: 'h' :: 'i' :: 's' :: 'i' :: 's' :: 'n' :: Empty() + 'o' + 't' + 'a' + 't' + 'r' + 'e' + 'e'

      val l = f.foldRight(List[Char]()) { (c, l) =>
        c :: l
      }

      l mustEqual List('t', 'h', 'i', 's', 'i', 's', 'n', 'o', 't', 'a', 't', 'r', 'e', 'e')
    }
  }
}
