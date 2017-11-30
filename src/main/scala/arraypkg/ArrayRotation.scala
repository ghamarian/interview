package arraypkg

object ArrayRotation {

  var tmp = 0;

  def swap(a: Array[Array[Int]], srcr: Int, srcc: Int, dstr: Int, dstc: Int) {
    println (s"$srcr, $srcc, $dstr, $dstc")
    var tmpVar = a(dstr)(dstc)
    a(dstr)(dstc) = tmp
    tmp = tmpVar
  }

  def rotateImage(a: Array[Array[Int]]): Array[Array[Int]] = {
      var r = a(0).length - 1
      for (d <- 0 to Math.max(0, r - 3)) {
        var c = a.length - 1
        for (i <- 0 to a(d).length -  2 * d - 2) {

          println (s"d=$d, i=$i")
          tmp = a(d)(d + i)
          swap(a, d, d + i, d + i, c - d)
          swap(a, d + i, c - d, r - d, c - d - i)
          swap(a, r - d, c - d - i, r - d - i, d)
          swap(a, r - d - i, d, d, d + i)
          print("--------\n")
          println((a).deep.mkString(" "))
        }
      }
    a
  }
  def main(args: Array[String]): Unit = {
//    val a = Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12), Array(13, 14, 15, 16))
//    val a = Array(Array(1, 2, 3), Array(5, 6, 7), Array(9, 10, 11))
    val a = Array(Array(10,9,6,3,7), Array(6,10,2,9,7), Array(7,6,3,8,2), Array(8,9,7,9,9), Array(6,8,6,8,2) )
    println (rotateImage(a).deep.mkString(" "))
  }



}
