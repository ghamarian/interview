import scala.collection.mutable.Stack

object LinuxPath {


  def simplifyPath(path: String): String = {
    var stack = new Stack[String]

    for (elem <- path.split('/')) {
      if (elem.trim == ".." ) {
        if (stack.nonEmpty)
          stack.pop()
      }
      else if (elem != "." && elem.trim != "") stack.push(elem)
    }

    if (stack.nonEmpty) {
      "/" + stack.reverse.mkString("/")
    }
    else "/"
  }

  def simple2(path: String): String = {
    val paths = path.split('/').foldLeft(List.empty[String])((l, str) => {
      str match {
        case ".." => if (l.isEmpty) l else l.tail
        case "." | "" => l
        case s => s::l
      }
    })

    "/" + paths.reverse.mkString("/")
  }


  def main(args: Array[String]): Unit = {
    println(simplifyPath("/home/a/./x/../b//c/"))
    println(simple2("/home/a/./x/../b//c/"))
  }



}
