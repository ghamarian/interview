object Merge {

  class ListNode[T](x: T) {
    var value: T = x
    var next: Option[ListNode[T]] = None
  }

  def mergeTwoLinkedLists(l1: Option[ListNode[Int]], l2: Option[ListNode[Int]]): Option[ListNode[Int]] = {

    if (l1.isEmpty && l2.isEmpty) None else {
      val l1v = l1.map{_.value}.getOrElse(Int.MaxValue)
      val l2v = l2.map{_.value}.getOrElse(Int.MaxValue)

      val (head, tail) = if (l1v < l2v)
                             (l1, mergeTwoLinkedLists(l1.flatMap(_.next), l2))
                        else (mergeTwoLinkedLists(l1, l2.flatMap(_.next)), l2)
      head.get.next = tail
      head
    }
  }
}
