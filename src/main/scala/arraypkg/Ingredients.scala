package arraypkg

import scala.collection.immutable.ListMap

object Ingredients {
  def groupingDishes(dishes: Array[Array[String]]): Array[Array[String]] = {
    val goodMap = for {
      dish <- dishes
      h = dish.head
      ingList = dish.tail
      ing <- ingList
    } yield ing -> h

    val stringToTuples = goodMap.groupBy {_._1}.map{ case (k,v) => v.unzip }
    val stringToStrings = stringToTuples.filter{case(k, v) => v.length >= 2}
    val stringToStrings1 = stringToStrings.map {case (k, v) => (k(0), v.sorted)}.toSeq.sortBy(_._1)


    val array = stringToStrings1.map{ case (k, v) =>  k +: v}.toArray

    println(array.deep.mkString(" "))

    println(stringToStrings.map{ case (k, v) => {s"$k -> ${v.mkString(", ")}\n"}})

    stringToTuples.foreach(e => println(s"${e._1} -> ${e._2}"))

    Array()
  }

  def main(args: Array[String]): Unit = {
    val dishes = Array(Array("Salad", "Tomato", "Cucumber", "Salad", "Sauce"),
    Array("Pizza", "Tomato", "Sausage", "Sauce", "Dough"),
    Array("Quesadilla", "Chicken", "Cheese", "Sauce"),
    Array("Sandwich", "Salad", "Bread", "Tomato", "Cheese"))

    groupingDishes(dishes)

  }

}
