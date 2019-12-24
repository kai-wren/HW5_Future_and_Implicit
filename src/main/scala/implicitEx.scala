
import implicitEx.superString


object implicitEx{

  implicit class superString(s: String) { //defining implicit class to enhance String functionality

    def -(v: String): String = (s.toFloat - v.toFloat).toString //defining subtraction method

    def *(v: String): String = (s.toFloat * v.toFloat).toString //defining multiplication

    def /(v: String): String = (s.toFloat / v.toFloat).toString //defining division

    def +++(v: String): String = (s.toFloat + v.toFloat).toString //defining summation. Since + and ++ already defined for Strings, I have to choose +++ as method name

  }

}

object testImplicit extends App{ //testing result

  def ~=(x: String, y: Double, precision: Double) = { //defining method to compare floats
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


