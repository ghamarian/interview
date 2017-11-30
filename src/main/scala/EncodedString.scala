import scala.annotation.tailrec

object EncodedString {

  sealed trait Token
  case class Open() extends Token
  case class Close() extends Token
  case class Num(n: Int) extends Token
  case class Text(str: String) extends Token {
    override def toString: String = str
  }
  case class Empty() extends Token

  def mergeTillNumber(t: Text, acc: List[Token]): List[Token] = {
    val next = acc.head
     next match {
      case Num(i) => Text(t.str * i) :: acc.tail
      case Text(str) => mergeTillNumber(Text(str+t.str), acc.tail)
      case _ => List()
    }
  }

  def decodeString(s: String): String = {
    val tokens = split(s).foldLeft(List.empty[Token])((acc, t) => {
      t match {
        case Num(i) => Num(i) :: acc
        case Text(str) =>
          Text(str) :: acc
        case Close() =>
          acc.head match {
            case tt@Text(str) => mergeTillNumber(tt, acc.tail)
          }
        case Open() => acc
      }
    })
    tokens.reverse.mkString
  }

  def split(s: String): List[Token] = {
    @tailrec
    def helper(s: String, res: List[Token]): List[Token] = {
      if (s.isEmpty) res.reverse
      else {
        val (t, rest) = next(s)
        helper(rest, t :: res)
      }
    }

    helper(s, List.empty)
  }

  def next(s: String): (Token, String) = {
    s.toList match {
      case Nil => (Empty(), "")
      case '[' :: cs => (Open(), cs.mkString)
      case ']' :: cs => (Close(), cs.mkString)
      case l@(c :: cs) if c.isDigit =>
        val (a, b) = l.span(_.isDigit)
        (Num(Integer.valueOf(a.mkString)), b.mkString)
      case l =>
        val (a, b) = l.span(_.isLetter)
        (Text(a.mkString), b.mkString)
    }
  }

  def main(args: Array[String]): Unit = {
//    println(split("100[codefights]").mkString)
//    val a = "z1[y]zzz2[abc]"
    val a = "1[ab]1[cd]"
    println(split(a).mkString)
    println(decodeString(a))
  }
}
