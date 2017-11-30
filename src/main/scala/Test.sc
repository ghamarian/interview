
def sq(x: Long) = x * x

sq(10)

val i = 10

val lng: Long = 10L

val xs = List(1, 2, 3, 4)

sq(lng)

sq(i)


val str = "amir"

str charAt 1

case class Rational(i: Int, n:Int = 1)

object Rational {
  implicit def intToRational(i: Int): Rational = Rational(i)
}



