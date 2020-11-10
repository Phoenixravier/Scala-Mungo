package compilerPlugin
//import Cats.{Cat => Cat}
//import Dogs.{Cat => Dog}
class Typestate(protocolName: String) extends scala.annotation.StaticAnnotation

@Typestate("CatProtocol")
class Cat {
  def m(): Unit ={
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
  val cat = new Cat()
  val dog = new Dog()
  cat.m()
  dog.walk()
  dog.walk()
  val scope = new Cat()
}

