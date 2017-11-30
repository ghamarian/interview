import scala.reflect.ClassTag

object Substring {

  case class Trie(v: Option[String], m: Map[Char, Trie]) {

    def addInner(t: Trie, s: List[Char], p: String): Trie = s match {
      case c::cs =>
        val newNode = t.m.getOrElse(c, Trie(Some(c + p), Map.empty))
        val newM = t.m.updated(c, addInner(newNode, cs, p + c))
        Trie(t.v, newM)
      case Nil  => t
    }
  }

  def main(args: Array[String]): Unit = {
  }

}
