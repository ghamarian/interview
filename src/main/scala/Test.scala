class Test {

  sealed trait Trie[+A]

  case class Leaf[+A] (key: Int, a: A) extends Trie[A]

  case class Branch[+A](prefix: Int,
                        mask: Int,
                        left: Trie[A],
                        right: Trie[A])

  case object Empty extends Trie[Nothing]
}
