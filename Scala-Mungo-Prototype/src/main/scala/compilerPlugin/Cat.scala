package compilerPlugin

import scala.sys.process._
import scala.language.postfixOps

import java.io.File

class Mammal(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename="src\\main\\scala\\ProtocolDSL\\Example.scala")
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
  System.getProperty("java.class.path")
  val cat = new Cat()
  cat.comeAlive()
  cat.run()

  "test.bat".!


}
