package compilerPlugin

import scala.util.Random

/*
class Typestate(filename:String) extends scala.annotation.StaticAnnotation

sealed trait DeathState
case object Dead extends DeathState
case object Alive extends DeathState
case object Unsure extends DeathState

@Typestate(filename = "MyProtocol.scala")
object Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def walk(): Unit = println("walking")
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

object Main extends App {
  Cat.comeAlive()
  Cat.walk()
  Cat.comeAlive()
}
*/