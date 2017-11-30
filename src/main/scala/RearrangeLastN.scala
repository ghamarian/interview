object RearrangeLastN {

  class ListNode[T](x: T) {
    var value: T = x
    var next: Option[ListNode[T]] = None
  }

  //while traversing the list pay attention to the loop invariants
  def rearrangeLastN(l: Option[ListNode[Int]], n: Int): Option[ListNode[Int]] = {
    var p = l
    var i = 0
    while (p.isDefined && i < n) {
      p = p.flatMap(_.next)
      i += 1
    }
    var res = l
    while (p.flatMap(_.next).isDefined) {
      p = p.flatMap(_.next)
      res = res.flatMap(_.next)
    }
    p.get.next = l
    val next = res.get.next
    res.get.next = None
    next
  }

  def mkString(l: Option[ListNode[Int]]): String = {
    l match {
      case None => ""
      case Some(lt) => lt.value + ", " + mkString(lt.next)
    }
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

    println(mkString(rearrangeLastN(l1, 3)))
  }

}
