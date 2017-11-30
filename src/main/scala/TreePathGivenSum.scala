object TreePathGivenSum {

  class Tree[T](x: T) {
    var value: T = x
    var left: Option[Tree[T]] = None
    var right: Option[Tree[T]] = None
  }

  def hasPathWithGivenSum(t: Option[Tree[Int]], s: Int): Boolean = {

    def isLeaf(t: Option[Tree[Int]]): Boolean = t.isDefined && t.flatMap(_.left).isEmpty && t.flatMap(_.right).isEmpty

    def helper(t: Option[Tree[Int]], k: Int): Boolean = {
      if (isLeaf(t) && k == 0) true
      else {
        helper(t.flatMap(_.left), k - t.get.value) || helper(t.flatMap(_.right), k - t.get.value)
      }
    }

    def mine: Boolean = {
      val a = Array(10, 12)
      a ++ Array(12)
      true
    }

    helper(t, s)
  }
}

