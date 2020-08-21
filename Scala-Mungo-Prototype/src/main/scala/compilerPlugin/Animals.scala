package compilerPlugin

import scala.language.postfixOps
import scala.util.Random

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

class Nonsense(filename:String) extends scala.annotation.StaticAnnotation

sealed trait DeathState
case object Dead extends DeathState
case object Alive extends DeathState
case object Unsure extends DeathState



object doMainThings {
  def main(args: Array[String]) {
    val cat = new Cat()
    val typedCat: Cat = new Cat("hi")
    var someCat = new Cat()
    var typedSomeCat: Cat = new Cat()
    val notACat: Int = 1
    var stillNotACat = "new Cat"

    cat.walk()
    cat.run()
    cat.createDog("wefdgf")
    cat.run()

    typedCat.walk()

    someCat.die()

    //cat.die()


    Dog.laze()
    Dog.stayOnAlert(false)
    //koira.cry()


    val jump = "false"
    Dog.stayOnAlert(jump, 2)


    def donothing(): Unit = {
      println("noting")
    }

    def main(deathState: DeathState): Unit = {
      println("dammit")
    }

    println("ha")
  }

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
    def this(s:String)={
      this
    }

    def createDog(str:String)={
      val doggo = Dog
      doggo
    }
  }

  @Nonsense(filename="fox.txt")
  class Rubish{

  }

  @Typestate(filename="src\\main\\scala\\ProtocolDSL\\DogProtocol.scala")
  object Dog extends Serializable{
    def walk():Unit = println("Yee kavelemme!")
    def cry():Unit = println("Itkeen :'(")
    def bark():Unit = println("hau hau")
    def laze():Unit = println("Olen vasinyt")
    def stayOnAlert(intruderHere:Boolean): Unit = {
      if(intruderHere) bark()
      else laze()
    }
    def stayOnAlert(str:String, nb:Int): Unit ={
      println("on alert")
    }
  }



}




