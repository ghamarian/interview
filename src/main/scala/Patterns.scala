import scala.collection.mutable
object Patterns {

  def areFollowingPatterns(strings: Array[String], patterns: Array[String]): Boolean = {
    var map: mutable.Map[String, String] = mutable.Map.empty

    for (elem <- patterns.zip(strings)) {
      val (p, s) = elem
      if (!map.isDefinedAt(p)) {
        map.put(p, s)
      }
      if (map(p) != s)  false
    }
    map.size == map.values.toSet.size //check the duplicate values.
  }

  //brilliant solution, not mutable
  def toS(x: Array[String]): Iterable[Set[Int]] = {
    val a = x.zipWithIndex.groupBy(_._1)
    val b = a.values
    val c = b.map(v => v.map(_._2).toSet)
    c
  }

  def main(args: Array[String]): Unit = {
    println(toS(Array("a", "b", "a", "c", "d", "c")))
  }

}
