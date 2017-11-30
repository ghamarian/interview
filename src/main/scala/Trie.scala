//trait Trie[T] {
//  private var prefixCount: Int = 0
//  private var completeCount: Int = 0
//  val children = scala.collection.mutable.HashMap[T, TrieNode[T]]()
//
//  def insert(data: Seq[T]): Unit = {
//    data.headOption match {
//      case Some(head) =>
//        prefixCount += 1
//        children get head match {
//          case Some(t) => t.insert(data.tail)
//          case _ =>
//            children += (data.head -> TrieNode(data.head))
//            children(data.head).insert(data.tail)
//        }
//      case _ =>
//        completeCount += 1
//    }
//  }
//
//  def get(data: Seq[T]): Option[Trie[T]] = {
//    if (data.isEmpty) Some(this)
//    else children get data.head match {
//      case Some(t) => t.get(data.tail)
//      case _ => None
//    }
//  }
//
//  def getPrefixCount = prefixCount
//  def getCompleteCount = completeCount
//  def getCount = prefixCount + completeCount
//
//  def prettyPrint(limit: Int = 0, akku: String = "", level: Int = 0): String = {
//    val info = s"${"--" * level} ${akku} - $prefixCount/$completeCount\n"
//    if (children.isEmpty || level >= limit) info
//    else info +
//      children.toList
//        .sortBy(p =>
//          -p._2.getCount)
//        .map(p =>
//          p._2.prettyPrint(limit, akku + p._1, level + 1))
//        .mkString
//  }
//}
//
//case class TrieRoot[T]() extends Trie[T]
//
//case class TrieNode[T](value: T) extends Trie[T]