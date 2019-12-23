
import implicitEx.superString


object implicitEx extends App {

  implicit class superString(s: String) {

    def -(v: String): String = (s.toFloat - v.toFloat).toString

    def *(v: String): String = (s.toFloat * v.toFloat).toString

    def /(v: String): String = (s.toFloat / v.toFloat).toString

    def +++(v: String): String = (s.toFloat + v.toFloat).toString

  }

}

object test extends App{

  def ~=(x: String, y: Double, precision: Double) = {
    if ((x.toFloat - y).abs < precision) true else false
  }

  val result: String = "6" * "2" - "3"
  println(result)
  assert(~=(result, 9, 0.0001))

  val result1: String = "8" - "2" * "3"
  println(result1)
  assert(~=(result1, 2, 0.0001))

  val result2: String = "8" +++ "2" * "3" - "7"
  println(result2)
  assert(~=(result2, 7, 0.0001))

  val result3: String = "8" * "2" +++ "2" * "3" - "7" / "2" +++ "1" / "2"
  println(result3)
  assert(~=(result3, 19, 0.0001))
}


