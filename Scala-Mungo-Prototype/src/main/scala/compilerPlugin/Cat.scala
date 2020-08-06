package compilerPlugin

import scala.language.postfixOps
import scala.util.Random

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
class Cat{
  def newMeth(s:String):Unit = println("test")
  def comeAlive(s:String, i:Int):String = "alternative come alive"
  def comeAlive():Unit = println("The cat is alive")
  def run():Unit = println("Running")
  def rest():Unit = println("Resting")
  def walk():Unit = println("Walking")
  def sleep():Unit = println("Sleeping")
  def die():DeathState = {
    val randomGenerator = Random
    val randomNumber = randomGenerator.nextDouble()
    println(randomNumber)
    if(randomNumber < 0.25) Dead
    else if(randomNumber < 0.5) Alive
    else if(randomNumber < 0.75) Unsure
    else null
  }
}

sealed trait DeathState
case object Dead extends DeathState
case object Alive extends DeathState
case object Unsure extends DeathState


object doThings {
  def main(args: Array[String]) {
    val cat = new Cat()
    cat.comeAlive()
    cat.run()
    println(cat.die())
  }

  def donothing(): Unit ={
    println("noting")
  }

  def main(): Unit ={
    println("dammit")
  }
  println("ha")

}
