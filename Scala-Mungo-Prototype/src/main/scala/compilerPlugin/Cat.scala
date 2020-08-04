package compilerPlugin

import scala.language.postfixOps
import scala.sys.process._

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

class Mammal(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename="src\\main\\scala\\ProtocolDSL\\Example2.scala")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def run(): Unit = println("Running")
  def rest(): Unit = println("Resting")
  def walk(): Unit = println("Walking")
  def sleep(): Unit = println("Sleeping")
  def die(): Boolean = {
    println("The cat is dead")
    true
  }
}


object main2 extends App {
  val cat = new Cat()
  cat.comeAlive()
  cat.run()

  "justEcho.bat".!
  println("executed after batch file")
}
