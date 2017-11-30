object RestoreBinary {

   class Tree[T](x : T) {
     var value: T = x
     var left: Option[Tree[T]] = None
     var right: Option[Tree[T]] = None
   }

  def restoreBinaryTree(inorder: Array[Int], preorder: Array[Int]): Option[Tree[Int]] = {

    val rootValue = preorder(0)
    val rootIndex = inorder.indexOf(rootValue)
    val inOrderLeft = inorder.slice(0, rootIndex)
    val inOrderRight = inorder.slice(rootIndex + 1, inorder.length)
    val preOrderLeft = preorder.slice(1, rootIndex)
    val preOrderRight = preorder.slice(rootIndex + 1, preorder.length)
    val root = new Tree(rootValue)

    root.left = restoreBinaryTree(inOrderLeft, preOrderLeft)
    root.right = restoreBinaryTree(inOrderRight, preOrderRight)

    Some(root)
  }
}

