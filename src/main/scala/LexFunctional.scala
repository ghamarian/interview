object LexFunctional {

  def createConnectedComponents(length: Int, pairs: Array[Array[Int]]): Set[Set[Int]] = {
    val sets = (0 until length).map(i => (i,i))
    val allPairs = pairs.toList.flatMap(iA => Set((iA(0)-1, iA(1)-1), (iA(1)-1, iA(0)-1)))
    val components: Map[Int, Set[Int]] = (allPairs ++ sets).groupBy(_._1).mapValues(_.map(_._2).toSet)

    def explore(t: Map[Int, Set[Int]], results: Set[Int], index: Int): Set[Int] = {
      val diff: Set[Int] = t(index).diff(results)
      if (diff.isEmpty) {
        results
      } else {
        diff.foldLeft(results) { case (res, i) => explore(t, res + i, i) }
      }
    }

    val connected = components.map {
      case (i, vs) => explore(components, Set(i), i)
    }.toSet

     println(sets)
     println(allPairs)
     println(components)
     println(s"Connected: $connected")
     println(explore(components, Set(components.keys.head), components.keys.head))

    connected
  }

  def swapLexOrder(str: String, pairs: Array[Array[Int]]): String = {
    val c = createConnectedComponents(str.length, pairs)
    val cc = c.toList.map(_.toList.sorted)
    val q = cc.flatMap { l =>
      val ll = l.map(i => str(i) -> i)
      val (lx, ix) = ll.unzip
      val s = lx.sorted.reverse
      val transformed = s.zip(ix).map { case (x,y) => (y,x) }
      val sss = transformed
      sss
    }
    val d = q.sortBy(_._1).map(_._2).mkString
    d
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
