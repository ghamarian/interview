object Engineer {

  //write something that generates a string that has all the bits.

  def another(level: Int, pos: Int): Int = {
    val newPos = pos - 1
    val legend = (0 until level).map(i => newPos >> i & 1)
    legend.foldLeft(0)((acc, x) => {
      if (x == 1) (acc + 1) % 2 else acc
    })
  }


  def main(args: Array[String]): Unit = {
//    println(a)
//    println(genLevel2(3)(3))
//    println(2.toBinaryString)

//    println((0 to 7).map(i => 13 >>i & 1))
    println(another(30, 163126329))
//        println(another(3, 3))

  }

}
