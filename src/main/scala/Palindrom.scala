object Palindrom {

  //you see option, go for pattern matching.
  class ListNode[T](x: T) {
    var value: T = x
    var next: Option[ListNode[T]] = None


    override def toString = s"ListNode($value)"
  }

  def isListPalindrome(l: Option[ListNode[Int]]): Boolean = {
    true

  }

  def mkString(l: Option[ListNode[Int]]): String = {
    l match {
      case None => ""
      case Some(lt) => lt.value + ", " + mkString(lt.next)
    }
  }

  def main(args: Array[String]): Unit = {
    val l1 = Some(new ListNode[Int](11))
    val l2 = Some(new ListNode[Int](12))
    val l3 = Some(new ListNode[Int](13))
    val l4 = Some(new ListNode[Int](13))
    val l5 = Some(new ListNode[Int](12))
    val l6 = Some(new ListNode[Int](11))

    l5.get.next = l6
    l4.get.next = l5
    l3.get.next = l4
    l2.get.next = l3
    l1.get.next = l2

    println(isPalindrom(l1))
  }

  def length(l: Option[ListNode[Int]]): Long = {
    l match {
      case None => 0L
      case Some(lt) => 1L + length(lt.next)
    }
  }

  def isPalindrom(l: Option[ListNode[Int]]): Boolean = {

    def isPalindrom(left: Option[ListNode[Int]], size: Long): (Boolean, Option[ListNode[Int]]) = {
      left match {
        case None => (true, None)
        case Some(lt) => {
          if (size == 1) {
            (true, left.get.next)
          }
          else if (size == 0) {
            (true, left)
          }
//          else if (size == 2) {
//            val next = left.get.next.get
//            println(s"middle: ${left.get.value} == ${next.value}")
//            (left.get.value == next.value, next.next)
//          }
          else {
            val k = isPalindrom(lt.next, size - 2)
              k match {
              case (false, rightOpt) => (false, rightOpt)
              case (true, rightOpt) => {
                rightOpt match {
                  case None => (true, rightOpt)
                  case Some(right) => {
                    println(s"Non: ${right.value} == ${lt.value}")
                    if (right.value == lt.value) (true, right.next) else (false, right.next)
                  }
                }
              }
            }
          }
        }
      }
    }

    l match {
      case None => true
      case Some(lt) => {
        isPalindrom(Some(lt), length(l)) match {
          case (result, last) => result
        }
      }
    }
  }

  def reverse(l: Option[ListNode[Int]]): Option[ListNode[Int]] = {

    def reverseHelper(l: Option[ListNode[Int]]): (Option[ListNode[Int]], Option[ListNode[Int]]) = {
      l match {
        case None => (None, None)
        case Some(lt) => {
          reverseHelper(lt.next) match {
            case (head, last) => {
              last match {
                case None => {
                  (l, l)
                }
                case Some(n) => {
                  n.next = l
                  lt.next = None
                  (head, l)
                }
              }
            }
          }
        }
      }
    }

    l match {
      case None => None
      case Some(lt) => {
        reverseHelper(Some(lt)) match {
          case (head, last) => head
        }
      }
    }
  }
}
