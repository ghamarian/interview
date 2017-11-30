import scala.annotation.tailrec

object HugeNumbers {

  //2 points for reversing recursively
  //1. You can pass on prev
  //2. You fist.next still goes to the rest
  class ListNode[T](x: T) {
    var value: T = x
    var next: Option[ListNode[T]] = None
  }

  def add(first: String, second: String, carry: Int): (String, Int) = {
    val sum = first.toInt + second.toInt + carry
    (s"${sum % 10000}", sum / 10000)
  }

  def add(first: Option[ListNode[Int]], second: Option[ListNode[Int]], carry: Int): (Option[ListNode[Int]], Int) = {
    val zero = new ListNode(0)
    val sum = first.getOrElse(zero).value + second.getOrElse(zero).value
    (Some(new ListNode(sum % 10000)), sum / 10000)
  }

  def goDeep(a: Option[ListNode[Int]], b: Option[ListNode[Int]], carry: Int): Option[ListNode[Int]] = {
    val (sum, newCarry) = add(a, b, carry)
    var result = None
    a match {
      case None => b match {
        case None if carry == 0 => None
        case None => Some(new ListNode(carry))
        case Some(bval) =>
          sum.get.next = goDeep(a, bval.next, newCarry)
          sum
      }
      case Some(aval) => b match {
        case None =>
          sum.get.next = goDeep(aval.next, b, newCarry)
          sum
        case Some(bval) =>
          sum.get.next = goDeep(aval.next, bval.next, newCarry)
          sum
      }
    }
  }

  def reverseAmazing(curr: Option[ListNode[Int]], prev: Option[ListNode[Int]]): Option[ListNode[Int]] = {
    if (curr.isEmpty) prev else {
      val next = curr.get.next
      curr.get.next = prev
      reverseAmazing(next, curr)
    }
  }

  def amazing(a: Option[ListNode[Int]], b: Option[ListNode[Int]], carry: Int): Option[ListNode[Int]] = {
    if (a.isEmpty && b.isEmpty) return if (carry == 1) Some(new ListNode(carry)) else None
    //nice point (.value is a function) and you can map
    val sum = a.map(_.value).getOrElse(0) + b.map(_.value).getOrElse(0) + carry
    val node = new ListNode(sum)
    //even more amazing
    node.next = amazing(a.flatMap(_.next), b.flatMap(_.next), sum / 10000)
    Some(node)
  }


  def traverse(a: Option[ListNode[Int]], b: Option[ListNode[Int]], carry: Int) : Option[ListNode[Int]] = {
    val (sum, newCarry) = add(a, b, carry)
    (a, b) match {
      case (None, None) if carry == 0 => None
      case (None, None) => Some(new ListNode(carry))
      case (None, Some(bval)) => {
        sum.get.next = traverse(None, bval.next, newCarry)
        sum
      }
      case (Some(aval), None) => {
        sum.get.next = traverse(aval.next, None, newCarry)
        sum
      }
      case (Some(aval), Some(bval)) =>
        sum.get.next = traverse(aval.next, bval.next, newCarry)
        sum
    }
  }

  def stringify(a: Option[ListNode[Int]]): String = {
    a match {
      case None => ""
      case Some(l) => s"${l.value} ${stringify(l.next)}"
    }
  }

  def reverse(a: Option[ListNode[Int]]): Option[ListNode[Int]] = {
    a match {
      case None => None
      case Some(l) => l.next match {
        case None => a
        case Some(next) => {
          val h = reverse(l.next)
          a.get.next.get.next = a
          a.get.next = None
          h
        }
      }
    }
  }


  //this passes itself to the next one. Hence tailref. (probably a bit nicer)
  def reverse2(a: Option[ListNode[Int]]): Option[ListNode[Int]] = {

    @tailrec
    def helper(curr: Option[ListNode[Int]], prev: Option[ListNode[Int]]): Option[ListNode[Int]] = {
      curr match {
        case None => None
        case Some(l) => {
          l.next match {
            case None => {
              l.next = prev
              curr
            }
            case Some(n) =>
              val next = l.next
              l.next = prev
              helper(next, curr)
          }
        }
      }
    }

    helper(a, None)
  }


  def main(args: Array[String]): Unit = {

    val l1 = Some(new ListNode[Int](1))
    val l2 = Some(new ListNode[Int](2))
    val l3 = Some(new ListNode[Int](3))
    val l4 = Some(new ListNode[Int](4))

    val l5 = Some(new ListNode[Int](5))
    val l6 = Some(new ListNode[Int](6))

    l5.get.next = l6
    l4.get.next = l5
    l3.get.next = l4
    l2.get.next = l3
    l1.get.next = l2

    println(stringify(reverseAmazing(l1, None)))
  }


  def addTwoHugeNumbers(a: Option[ListNode[Int]], b: Option[ListNode[Int]]): Option[ListNode[Int]] = {

    //    def helper(a: Option[ListNode[Int]], b: Option[ListNode[Int]], carry: Int): Option[ListNode[Int]] = {
    //
    //
    //    }

    ???
  }


}
