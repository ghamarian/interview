object DeleteBinTree {

  //
  // Definition for binary tree:
  case class Tree[T](x: T) {
    var value: T = x
    var left: Option[Tree[T]] = None
    var right: Option[Tree[T]] = None

    override def toString: String =  {
      val leftTree = if (left.isEmpty) "" else s"(${left.get.toString})"
      val rightTree = if (right.isEmpty) "" else s"(${right.get.toString})"
      s"$leftTree.$value.$rightTree"
    }
  }

  def deleteFromBST(t: Option[Tree[Int]], queries: Array[Int]): Option[Tree[Int]] = {
    queries.foldLeft(t)((n, k) => deleteRec(n, k))
  }

  def findMax(node: Option[Tree[Int]]): Int = {
    if (node.get.right.isEmpty) node.get.value
    else findMax(node.get.right)
  }

  def max(node: Option[Tree[Int]]): Option[Tree[Int]] = {
    if (node.get.right.isEmpty) node
    else max(node.get.right)
  }

  def deleteRec(node: Option[Tree[Int]], k: Int): Option[Tree[Int]] = {
    if (node.isEmpty) node
    else if (k < node.get.value) {
      node.get.left = deleteRec(node.get.left, k)
    }
    else if (k > node.get.value) {
      node.get.right = deleteRec(node.get.right, k)
    }
    else {
      if (node.get.left.isEmpty) {
        return node.get.right
      }
      else if (node.get.right.isEmpty) {
        return node.get.left
      }
      else {
        val max = findMax(node.get.left)
        node.get.value = max
        node.get.left = deleteRec(node.get.left, max)
      }
    }
    node
  }

  def deleteAndReturnRightMost(parent: Option[Tree[Int]], node: Option[Tree[Int]]): Int = {
    if (node.get.left.isEmpty && node.get.right.isEmpty) {
      val value = node.get.value
      parent.get.right = None
      value
    }
    else {
      deleteAndReturnRightMost(node, node.get.right)
    }
  }

  def deleteOneNode(node: Option[Tree[Int]], parent: Option[Tree[Int]], k: Int): Unit = {
    if (node.isEmpty) node
    else if (node.get.value == k) {
      if (node.get.left.isDefined) {
        val left = node.get.left
        val right = left.get.right
        if (left.get.right.isDefined) {
          val value = deleteAndReturnRightMost(left, right)
          node.get.value = value
        }
      }
      else {
      }
    }
    else if (k < node.get.value)  deleteOneNode(node.get.left, node, k)
    else  deleteOneNode(node.get.right, node, k)
  }

  def main(args: Array[String]): Unit = {
    val root = Some(Tree(10))

    val left = Some(Tree(5))
    left.get.left = Some(Tree(4))
    val leftright = Some(Tree(8))
    leftright.get.left = Some(Tree(7))

    left.get.right = leftright

    root.get.left = left
    root.get.right = Some(Tree(15))

    println(root)
    println("------------------")
    println(deleteRec(root, 10))

//    println(root)
//    deleteKey(root, 3)
//    println(root)
//    deleteKey(root, 2)
//    println(root)
//    deleteKey(root, 1)
//    println(root)

//    val option = deleteFromBST(root, Array(3, 2, 1))
//    println(option.get)
  }
}

