package baysick
import scala.language.postfixOps
class TestClass {

}

object TestClass extends Baysick {
  def main(args:Array[String]): Unit = {
    10 PRINT "Enter a number"
    20 INPUT Symbol("n")
    30 PRINT "Square root of " % "n is " % SQRT(Symbol("n"))
    40 END "ha"

    RUN
  }
}