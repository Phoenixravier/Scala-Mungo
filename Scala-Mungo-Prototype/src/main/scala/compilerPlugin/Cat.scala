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
  def this(s:String){
    this
  }
}



sealed trait DeathState
case object Dead extends DeathState
case object Alive extends DeathState
case object Unsure extends DeathState


object doThings extends App{
  val cat = new Cat()
  val typedCat:Cat = new Cat("hi")
  var somecat = new Cat()
  var typedSomeCat:Cat = new Cat()
  val notACat:Int = 1
  var stillNotACat = "new Cat"
  cat.comeAlive()
  cat.run()
  println(cat.die())
  main(cat.die())
  for(i <- 4 to 8){
    cat.die()
  }

  def donothing(): Unit ={
    println("noting")
  }

  def main(deathState: DeathState): Unit ={
    println("dammit")
  }
  println("ha")

}
