package compilerPlugin


import scala.language.postfixOps

class Typestate(filename:String) extends scala.annotation.StaticAnnotation


object doMainThings {
  def main(args: Array[String]) {
    dothgm()
    val kat = new Cat()
    someFunction()
    makeCatWalk(kat)
    makeCatWalk(kat)
    walkWithNoCat()
    kat.walk()
    kat.comeAlive()

    def dothgm2(): Unit ={
      val cat = new Cat()
      cat.walk()
    }
    println("sdfd")
  }

  def init(){
  }


  def dothgm(): Unit ={
    val cat = new Cat()
    cat.walk()
    cat.walk()
  }

  def walkWithNoCat(): Unit ={
    println("walking in the void")
    class anotherItem(){

    }
  }

  def someFunction(): Unit ={
    println("in some function")
    goSomewhere()
    class item(){

    }
    def goSomewhere(): Unit ={
      def newF(): Unit ={
        println("Yet another function")
      }
      println("going somewhere else")
    }
    def canyouREachMeTraverser(): Unit ={
      def deeper(): Unit ={
        def deeperest(): Unit ={

        }
      }
    }

  }

  def goSomewhere(): Unit ={
    println("going somewhere")
  }


  def makeCatWalk(kitty:Cat): Unit ={
    kitty.walk()
  }

  def makeCatComeAlive(catToLive:Cat): Unit ={
    catToLive.comeAlive()
  }



  @Typestate(filename="src\\main\\scala\\ProtocolDSL\\DogProtocol.scala")
  class Dog extends Serializable{
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

@Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
class Cat{

  println("help")
  def selfChange(kit:Cat): Unit ={
    kit.walk()
  }
  def newMeth(s:String):Unit = println("test")
  def comeAlive(s:String, i:Int):String = "alternative come alive"
  def comeAlive():Unit = println("The cat is alive")
  def run():Unit = println("Running")
  def rest():Unit = println("Resting")
  def walk():Unit = println("Walking")
  def sleep():Unit = println("Sleeping")
  def this(s:String)={
    this
  }
}




