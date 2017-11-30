

object ContainsCloseNames {

  def badUnderstanding(nums: Array[Int], k: Int): Boolean = {
    val sorted = nums.sorted


    val tuples = sorted.zip(sorted.slice(1, sorted.length))

    println(tuples.map{case (i, j) => s"($i, $j)"}.deep.mkString(" "))
    for (elem <- tuples) {
      if (elem._1 +k > elem._2) true
    }
    false
  }

  def containsCloseNums(nums: Array[Int], k: Int): Boolean = {
    val intToTuples = nums.zipWithIndex.groupBy(_._1)

    val values = intToTuples.values
    println(values.mkString(" "))
    val all = values.map(v => v.map(_._2))
    val finalRes = all.map(v => v.zip(v.slice(1, v.length)))
    for (elems <- finalRes) {
      for (elem <- elems) {
        if (elem._2 - elem._1 <= k) return true
      }
    }
    false
  }

  def main(args: Array[String]): Unit = {
    println(containsCloseNums(Array(1, 2, 3, 4, 1, 6), 3))
  }
}
