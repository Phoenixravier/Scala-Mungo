package compilerPlugin


import scala.language.postfixOps

class Typestate(filename:String) extends scala.annotation.StaticAnnotation



object doMainThings{
  println("in main things")
  def main(args: Array[String]): Unit = {
    val cat = new Cat(0)
    Dog
  }
  println("still in main things")

  def makeCatWalk(set:Cat): Unit ={
    set.walk()
    set.walk()
  }

  object notMain{
    println("not main")
    println("don't execute this")
    val cat = new Cat(4)
    cat.walk()
    cat.walk()
  }

  //@Typestate(filename="src\\main\\scala\\ProtocolDSL\\DogProtocol.scala")
  object Dog extends Serializable{
    println("made a dog")
    val cat = new Cat(1)
    cat.walk()
    cat.walk()
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

object Trash{

}


@Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
class Cat(id:Int){
  println("init "+id)
  walk()
  walk()

  def selfChange(kit:Cat): Unit ={
    kit.walk()
  }
  def newMeth(s:String):Unit = println("test")
  def comeAlive(s:String, i:Int):String = "alternative come alive"
  def comeAlive():Unit = println("The cat is alive")
  def run():Unit = println("Running")
  def rest():Unit = println("Resting")
  def walk():Boolean = {
    println("walking "+id)
    false
  }
  def sleep():Unit = println("Sleeping")
}




