import scala.collection.mutable


object SubstringMutable {

  case class Trie(var v: Option[String] = None, m: mutable.HashMap[Char, Trie]) {

    def add(a: List[Char], t: Trie, sofar: String): Trie = {
      a match {
        case c :: cs =>
          val newNode = t.m.getOrElseUpdate(c, Trie(None, mutable.HashMap.empty))
          add(cs, newNode, sofar + c)
          t
        case Nil =>
          t.v = Some(sofar)
          t
      }
    }
  }

  def calcSubs(input: String, trie: Trie): List[(String, Int)] = {
    val result = input.zipWithIndex.toList.foldLeft((List.empty[(String, Int)], List.empty[(Option[Trie], Int)])) {
      case ((f, t), (s, i)) =>
        val tuples = ((Some(trie), i) :: t).map { case (x, index) => { (x.get.m.get(s), index) } }.filter(_._1.isDefined)
        val finalOnes = tuples.filter(_._1.get.v.isDefined).map{ case (matched, index) => (matched.get.v.get, index) }
        (finalOnes ++ f, tuples)
    }
    result._1
  }

  def write(trie: Trie)(input: String): String = {
    val tuples = calcSubs(input, trie)
    if (tuples.isEmpty) input
    else {
      val tuple = tuples.groupBy(_._1.length).maxBy(_._1)
      val (fmatch, start) = tuple._2.minBy(_._2)
      s"${input.take(start)}($fmatch)${input.drop(start + fmatch.length)}"
    }
  }

  def calcSubs2(input: String, trie: Trie): List[(Option[Trie], Int)] = {
    input.zipWithIndex.toList.foldLeft(List.empty[(Option[Trie], Int)]) { case (t, (s, i)) =>
      ((Some(trie), i) :: t).flatMap { case (x, index) => {
        if (x.get.v.isDefined && x.get.m.get(s).isDefined) List((x.get.m.get(s), index), (x, index))
        else if (x.get.v.isDefined) List((x, index))
        else if (x.get.m.get(s).isDefined) List((x.get.m.get(s), index))
        else List()
      }
      }.filter(_._1.isDefined)
    }
  }

  def findSubstrings(words: Array[String], parts: Array[String]): Array[String] = {
    val trie = parts.toList.map(_.toList).foldLeft(Trie(None, mutable.HashMap.empty))((t, x) => t.add(x, t, ""))
    words.map(write(trie))
  }


  def main(args: Array[String]): Unit = {
    val trie = List("a", "mel", "lon", "el", "kashk", "ashke").map(_.toList).foldLeft(Trie(None, mutable.HashMap.empty))((t, x) => t.add(x, t, ""))
    val names = Array("kashkel", "watermelon", "elon", "d")

    println(findSubstrings(names, Array("a", "mel", "lon", "el", "kashk", "ashke")).mkString(" "))
    println(findSubstrings(Array("pasteboard"), Array("st", "pe", "p")).mkString(" "))

    //  def write(trie: Trie)(input: String): String = {
    //    val tuples = calcSubs(input, trie)
    //    val matchIndexed = for {
    //      l1 <- tuples
    //      (t, i) = l1
    //      matched <- t
    //      if matched.v.isDefined
    //    } yield (t.get.v.get, i)
    //
    //    if (matchIndexed.isEmpty) input else {
    //      val tuple = matchIndexed.groupBy(_._1.length).maxBy(_._1)
    //      val (fmatch, start) = tuple._2.minBy(_._2)
    //      s"${input.take(start)}($fmatch)${input.drop(start + fmatch.length)}"
    //    }
    //  }

    //
    //    val maybeTrieses = names.map(calcSubs)
    //
    //    val allsubstrings = for {
    //      l1 <- maybeTrieses
    //    } yield for {
    //      l2 <- l1
    //      t <- l2._1
    //      if t.v.isDefined
    //    } yield (t.v.get, l2._2)
    //
    //    val tuples = maybeTrieses.flatMap{ a =>  a.map{case (mat, index) => (mat.get.v, index)}}
    //    val finalRes = allsubstrings.map(x => x.maxBy(_._1.length))
    //    println(finalRes)
    //
    //    val strings = for {
    //      some <- maybeTrieses
    //    } yield
    //      for {
    //        wordList <- some
    //        (a, b) = wordList
    //        sublist <- a
    //        v <- sublist.v
    //      } yield v
  }
}

