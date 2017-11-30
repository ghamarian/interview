object LexFunctional2 {

  def getSwapGroups(m:Map[Int,Set[Int]], i:Array[Int]):Map[Int,Set[Int]] = {
    val (a,b) = (i(0),i(1))
    val relevent = m.filter{case(k,v) => v.contains(a) || v.contains(b)}
    relevent.size match {
      case 0 => m + (a -> Set(a,b))
      case 1 => m + (relevent.head._1 -> (relevent.head._2 + a + b))
      case _ =>
        val keysToRemove = relevent.keySet
        val keyToStay = relevent.keySet.head
        val mergedValues = keysToRemove.foldLeft(Set[Int]())((s,k) => s ++ m(k)) + a + b
        keysToRemove.foldLeft(m)((m,k) => m - k) + (keyToStay -> mergedValues)
    }
  }

  def maximizeSwapGroup(indicies:Set[Int], s:String): List[(Char, Int)] = {
    indicies.toList.map(i => s(i - 1)).sorted.reverse.zip(indicies.toList.sorted)
  }

  def swapLexOrder(str: String, pairs: Array[Array[Int]]): String = {
    val graphGroups = pairs.foldLeft(Map[Int,Set[Int]]())((m,g) => getSwapGroups(m,g))
    val allG = graphGroups.values.foldLeft(Set[Int]())((s,b) => s++b)
    val stableValues = str.zipWithIndex.filterNot(c => allG.contains(c._2 + 1))
    val allValues = graphGroups.values.toList.flatMap(maximizeSwapGroup(_,str)) ++ stableValues
    allValues.sortBy(_._2).map(_._1).mkString("")
  }

  def main(args: Array[String]): Unit = {

    val str= "qvspxdrbvwfuaahtzbpjudfyzccgzwynkgihwmdshvfnvyvfjc"
    val pairs = Array(
      Array(16,26),
      Array(2,25),
      Array(25,27),
      Array(19,20),
      Array(13,20),
      Array(4,26),
      Array(19,27),
      Array(18,26),
      Array(13,23),
      Array(1,4),
      Array(11,19),
      Array(16,19),
      Array(25,28),
      Array(19,30),
      Array(19,25),
      Array(1,11),
      Array(2,20),
      Array(10,22),
      Array(6,19),
      Array(7,26),
      Array(3,30),
      Array(15,23),
      Array(12,26),
      Array(1,3),
      Array(3,12),
      Array(3,26),
      Array(16,30),
      Array(2,16),
      Array(4,13))

    println(swapLexOrder(str, pairs))

  }

}
