object Crypto {

  def isCryptSolution(crypt: Array[String], solution: Array[Array[Char]]): Boolean = {

    def convert(input: String, m: Map[Char, Int]): Long = {
      val coefs: Stream[Long] = Stream.from(0) map(math.pow(10,_).toLong)

      println ((coefs take 5).mkString(" "))

      val tuples = (input.reverse map m).zip(coefs)
      val ints = tuples.map { case (k, v) => k * v }
      ints.sum
    }

    val tuples = for {
      s <- solution
    } yield s(0) -> (s(1) - '0')

    val solMap = tuples.toMap

    val charCrypt = crypt.map(_.toArray)

    //    !crypt(0).zip(crypt(1)).exists { case (k, v) => k == v }&&

    val i  = convert(crypt(0), solMap)
    val i1 = convert(crypt(1), solMap)
    val i2 = convert(crypt(2), solMap)
    println(s"i=$i, i1=$i1, i2=$i2")

    val bool = !((i.toString.length == i1.toString.length) && (i.toString.length < crypt(0).length)) &&
      !((i == 0L || i2 == 0) && i2 != 0) &&
      i + i1 == i2

    println(bool)
    bool
  }

  def main(args: Array[String]): Unit = {
//    isCryptSolution(Array("SEND", "MORE", "MONEY"),
//           Array(Array('O', '0'),
//           Array('M','1'),
//           Array('Y','2'),
//           Array('E','5'),
//           Array('N','6'),
//           Array('D','7'),
//           Array('R','8'),
//           Array('S','9')))

    isCryptSolution(Array("AA", "BB", "AA"), Array(Array('A', '1'), Array('B', '0')))
//      isCryptSolution(Array("FOUR", "FOUR", "EIGHT"),
//        Array( Array('F','5'),
//        Array('O','2'),
//        Array('U','3'),
//        Array('R','9'),
//        Array('E','1'),
//        Array('I','0'),
//        Array('G','4'),
//        Array('H','7'),
//        Array('T','8')))

  }
}