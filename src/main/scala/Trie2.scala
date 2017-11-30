case class Trie2(v: Option[String], m: Map[Char, Trie2]) {

  def addInner(t: Trie2, s: List[Char], p: String): Trie2 = s match {
    case c :: cs => Trie2(t.v, t.m.updated(c, addInner(t.m.getOrElse(c, Trie2(Some(p + c), Map.empty)), cs, p + c)))
    case Nil => t
  }

  def add(t: Trie2, s: String): Trie2 = addInner(t, s.toList, "")

  def findInner(t: Trie2, s: List[Char]): Option[String] = s match {
    case Nil => t.v
    case c :: cs => t.m.get(c).flatMap { t => findInner(t, cs) }
  }

  def find(t: Trie2, s: String): Option[String] = findInner(t, s.toList)

  val root = Trie2(None, Map.empty)
}

