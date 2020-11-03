/*
package compilerPlugin

import scala.util.control.Breaks

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename = "src\\main\\scala\\ProtocolDSL\\SomeProtocol.scala")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def walk(): Unit = println("walking")
}

object main extends App {
  val cat = new Cat()
  val Inner = new Breaks
  val Outer = new Breaks
  var y = 0
  Outer.breakable {
    do {
      Inner.breakable {
        val x = 0
        x match {
          case 0 =>
            cat.comeAlive()
            cat.comeAlive()
            Inner.break()
          case 1 =>
            cat.walk()
            Outer.break()
        }
      }
    }while(y == 0)
  }
}


 */


