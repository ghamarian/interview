object KthSmallest {

   class Tree[T](x : T) {
     var value: T = x
     var left: Option[Tree[T]] = None
     var right: Option[Tree[T]] = None
   }

  def kthSmallestInBST(t: Option[Tree[Int]], k: Int): Int = {

    def helper(t: Option[Tree[Int]], a: Int): (Int, Boolean) = {
      if (t.isEmpty) (0, false)
      else {
        val (leftSize, result) = helper(t.flatMap(_.left), a)
        if (result) (leftSize, true)
        else if (a  == leftSize + 1) (t.get.value, true)
        else helper(t.flatMap(_.right), a - leftSize - 1)
        //some missing part (pass info up)
      }
    }
    helper(t, k)._1
  }

  def main(args: Array[String]): Unit = {
    //to really make a change. (at least one month of eight hours of practice. makes you a better programmer.)
  }

}
