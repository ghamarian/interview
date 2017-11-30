import scala.collection.mutable.Stack
object NextLarger {

  def nextLarger(a: Array[Int]): Array[Int] = {
    val s = new Stack[Int]
    val res = Array.fill(a.length)(-1)

    s.push(0)
    for (elem <- 1 until a.length) {
      while (s.nonEmpty && a(elem) > a(s.top)) {
        val top = s.pop()
        res(top) = a(elem)
      }
      s.push(elem)
    }
    res
  }

  def main(args: Array[String]): Unit = {
    println(nextLarger(Array(1, 3, 2, 4)).mkString(", "))
  }
}


