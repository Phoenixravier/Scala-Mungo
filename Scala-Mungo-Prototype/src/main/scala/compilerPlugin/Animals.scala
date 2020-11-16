package compilerPlugin

import Dogs.Kitten
import compilerPlugin.letters.letter

//import Cats.{Cat => Cat}
//import Dogs.{Cat => Dog}

class Typestate(filename: String) extends scala.annotation.StaticAnnotation

object letters extends Enumeration {
  type letter = Value
  val A,B,C,D = Value
}

@Typestate("CatProtocol")
class Cat(nb:Int) {
  def m(): letter ={
    letters.A
  }
  def jump(): Unit ={

  }
  def grab(): Unit ={

  }
  def go(): Unit ={

  }
  def stop(): Unit ={

  }
}

@Typestate("DogProtocol")
class Dog {
  def walk():Unit = println("Jee kävelemme!")
  def cry():Unit = println("Itkeen :'(")
  def bark():Unit = println("hau hau")
  def laze():Unit = println("Olen väsynyt")
  def stayOnAlert(intruderHere:Boolean): Unit = {
    if(intruderHere) bark()
    else laze()
  }
  def stayOnAlert(str:String, nb:Int): Unit ={
    println("on alert")
  }
}

object main extends App{
  val cat = new Cat(1)
  val dog = new Dog()
  dog.walk()
  Kitten
  Kitten
}

