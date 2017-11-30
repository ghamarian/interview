import scala.collection.mutable

object KthElement {

  def kthLargestElement(nums: Array[Int], k: Int): Int = {
   val heap = mutable.PriorityQueue(nums:_*)
    for (elem <- 1 until k) {
      heap.dequeue()
    }
    heap.dequeue()
  }
}
