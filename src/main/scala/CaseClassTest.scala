case class CaseClassTest(a: Int) {
  val b = 2
}

object Main {
  def main(args: Array[String]): Unit = {
    val c = CaseClassTest(10)
    println(c)
  }
}
