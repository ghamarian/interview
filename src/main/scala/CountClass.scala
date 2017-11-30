object CountClass {

  def dfs(skyMap: Array[Array[Char]], i: Int, j: Int): Set[(Int, Int)] = {
    var res: Set[(Int, Int)] = Set.empty
    val r = skyMap.length - 1
    val c = skyMap(0).length - 1

    def helper(irow: Int, icol: Int, sofar: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (skyMap(irow)(icol) == '0') sofar
      else {
        var newsofar = sofar + (irow -> icol)
        if (irow < r)
         newsofar = newsofar ++ helper(irow + 1, icol, newsofar)
        if (icol < c)
         newsofar = newsofar ++ helper(irow, icol + 1, newsofar)
        newsofar
      }
    }
    helper(i, j, Set.empty)
  }

  def countClouds(skyMap: Array[Array[Char]]): Int = {
//    countClouds()
  }

  def main(args: Array[String]): Unit = {
    val skyMap = Array(Array('0', '1', '1', '0', '1'),
                       Array('0', '1', '1', '1', '1'),
                       Array('0', '0', '0', '0', '1'),
                       Array('1', '0', '0', '1', '1'))
    println(dfs(skyMap, 0, 1).mkString(" "))
  }
}
