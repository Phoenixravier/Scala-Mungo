package compilerPlugin

import scala.language.postfixOps

class Typestate(filename:String) extends scala.annotation.StaticAnnotation


object doMainThings {
  def main(args: Array[String]) {
    val cat = new Cat()
    someFunction()
    makeCatWalk(cat)
    makeCatWalk(cat)
    walk()
    cat.comeAlive()
  }

  def walk(): Unit ={
    println("walking in the void")
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

    }

  }

  def goSomewhere(): Unit ={
    println("going somewhere")
  }


  def makeCatWalk(cat:Cat): Unit ={
    cat.walk()
  }

  def makeCatComeAlive(cat:Cat): Unit ={
    cat.comeAlive()
  }


  @Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
  class Cat{
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




