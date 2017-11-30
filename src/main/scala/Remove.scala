object Remove {

   class ListNode[T](x : T) {
     var value: T = x
     var next: Option[ListNode[T]] = None
   }

  def removeKFromList(l: Option[ListNode[Int]], k: Int): Option[ListNode[Int]] = {
    if (l.isEmpty) return l
    if (l.get.value == k) {
      removeKFromList(l.get.next, k)
    }
    else {
      if (l.get.next.isEmpty) l
      else {
        l.get.next = removeKFromList(l.get.next, k)
        l
      }
    }
  }

  //you see option, then go for pattern matching.
  def removeKFromList2(l: Option[ListNode[Int]], k: Int): Option[ListNode[Int]] = {
    l match {
      case None => None
      case Some(lt) =>
        if (lt.value == k) {
          removeKFromList(lt.next, k)
        }
        else {
          lt.next = removeKFromList(lt.next, k)
          l
        }
    }
  }





  def main(args: Array[String]): Unit = {
    val l = Some(new ListNode[Int](10))
    l.get.next = Some(new ListNode[Int](10))

    removeKFromList(l, 10)
  }

}
