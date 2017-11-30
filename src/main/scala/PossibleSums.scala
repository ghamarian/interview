object PossibleSums {

  def possibleSums(coins: Array[Int], quantity: Array[Int]): Int = {
    def helper(coins: Array[Int], quantity: Array[Int], allSoFar: Set[Int]): Set[Int] = {

      if (quantity.length == 0 || quantity.sum == 0) return Set(0)
      val coinsTail = coins.slice(1, coins.length)
      val quantityTail = quantity.slice(1, quantity.length)
      val newSet = helper(coinsTail, quantityTail, allSoFar)

      val sum = coinsTail.zip(quantityTail).map {case(k, v) => k*v}.sum

      val ints = for {
        q <- 0 to quantity(0)
        s <- newSet
      } yield q * coins(0) + s

      println("set so far is " + ints.mkString(" "))

      ints.toSet
    }
    helper(coins, quantity, Set.empty).size - 1
  }

  def main(args: Array[String]): Unit = {
    println(possibleSums(Array(10, 50, 100), Array(1, 2, 1)))
  }

}
