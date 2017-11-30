import scala.collection.mutable

object MinimumOnStack {


  import scala.collection.mutable.Stack

  def minimumOnStack(operations: Array[String]): Array[Int] = {
    val s = new mutable.ArrayStack[Int]
    val min = new mutable.ArrayStack[Int]

    val push = """push (\d+)""".r
    var queue = new mutable.Queue[Int]()

    for (elem <- operations) {
      elem match {
        case push(d) =>
          val value = Integer.valueOf(d)
          if (s.isEmpty) {
            min.push(value)
          }
          else {
              min.push(Math.min(value, min.top))
          }
          s.push(value)
        case "pop" =>
          s.pop
          min.pop
        case "min" => queue += min.top
      }
    }
    queue.toArray
  }

  def main(args: Array[String]): Unit = {
    println(minimumOnStack(Array("push 10", "min", "push 5", "min", "push 8", "min", "pop", "min", "pop", "min")).mkString(" "))
  }


}
