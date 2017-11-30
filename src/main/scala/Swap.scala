import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Swap {

  def findSlot(mem: ArrayBuffer[mutable.Set[Int]], elem: Array[Int]): mutable.Set[Int] = {
    val first = mem.zipWithIndex.find(k => k._1.contains(elem(0)))
    val second = mem.zipWithIndex.find(k => k._1.contains(elem(1)))

    val firstIndex = first.map(_._2)
    val secondIndex = second.map(_._2)

    (firstIndex, secondIndex) match {
      case (None, None) => {
        val a = new mutable.TreeSet[Int]()
        mem += a
        a
      }
      case (_, None) => first.map(_._1).get
      case (None, _) => second.map(_._1).get
      case (Some(aval), Some(bval)) if aval != bval => {
        mem(aval) ++= mem(bval)
        val result = mem(aval)
        mem.remove(bval)
        result
      }
      case (Some(aval), _) => mem(aval)
    }
  }

  def sortSubstring(str: String, index: Array[Int]): String = {
    val array = str.toCharArray
    var stringBuffer: mutable.StringBuilder = new mutable.StringBuilder()
    for (elem <- index) {
      stringBuffer += array(elem)
    }

    val sorted = stringBuffer.sorted.reverse
    for (elem <- index.zipWithIndex) {
      array(elem._1) = sorted.charAt(elem._2)
    }
    new String(array)
  }

  def create(pairs: Array[Array[Int]]): ArrayBuffer[mutable.Set[Int]] = {
    val mem: ArrayBuffer[mutable.Set[Int]] = new ArrayBuffer()

    for (elem <- pairs) {
      var ints = findSlot(mem, elem)
      ints.add(elem(0))
      ints.add(elem(1))
    }
    mem
  }

  def swapLexOrder(str: String, pairs: Array[Array[Int]]): String = {
    val k = pairs.map(v => v.map(_ - 1))
    val pairSet = create(k)
    var result = str
    for (elem <- pairSet) {
      result = sortSubstring(result, elem.toArray)
    }
    result
  }

  def main(args: Array[String]): Unit = {

    println(sortSubstring("abcd", Array(1, 3)))
    val rel = Array(Array(13, 23), Array(13, 28), Array(15, 20), Array(24, 29), Array(6, 7), Array(3, 4), Array(21, 30), Array(2, 13), Array(12, 15), Array(19, 23), Array(10, 19), Array(13, 14), Array(6, 16), Array(17, 25), Array(6, 21), Array(17, 26), Array(5, 6), Array(12, 24))
    val amir = create(rel)

    println(amir.flatten.sorted.mkString(" "))
    println(rel.flatten.sorted.distinct.mkString(" "))

//    println(swapLexOrder("lvvyfrbhgiyexoirhunnuejzhesylojwbyatfkrv", rel))

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

    println(create(pairs).flatten.sorted.mkString(" "))
    println(pairs.flatten.sorted.distinct.mkString(" "))

    println(swapLexOrder(str, pairs))

  }

}
