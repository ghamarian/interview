object ReverseKGroups {

  class ListNode[T](x: T) {
    var value: T = x
    var next: Option[ListNode[T]] = None
  }

  //this instead can go to the node itself instead of the next node. (will be more concise)
  def findAtMost(l: Option[ListNode[Int]], k: Int): (Boolean, Option[ListNode[Int]]) = {
    if (l.isEmpty || k == 0) (k == 0, l) else {
      findAtMost(l.get.next, k - 1)
    }
  }

  def reverse(cur: Option[ListNode[Int]], prev: Option[ListNode[Int]], end: Option[ListNode[Int]]): Option[ListNode[Int]] = {
    if (cur == end) prev else {
      val next = cur.get.next
      cur.get.next = prev
      reverse(next, cur, end)
    }
  }

  def reverseNodesInKGroups(l: Option[ListNode[Int]], k: Int): Option[ListNode[Int]] = {

    def helper(cur: Option[ListNode[Int]], prev: Option[ListNode[Int]]): Option[ListNode[Int]] = {
      if (cur.isEmpty) None else {
        val (enough, nextK) = findAtMost(cur, k)
        if (enough) {
          val result = reverse(cur, None, nextK)
          if (prev.isDefined) prev.get.next = result
          helper(nextK, cur)
          result
        } else {
          if (prev.isDefined) prev.get.next = cur
          cur
        }
      }
    }

    helper(l, None)
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

    println(mkString(reverseNodesInKGroups(l1, 2)))
  }
}
