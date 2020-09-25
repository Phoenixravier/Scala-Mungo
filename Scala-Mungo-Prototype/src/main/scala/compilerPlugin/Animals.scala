package compilerPlugin


import java.io.{FileNotFoundException, FileOutputStream, IOException, ObjectOutputStream}



import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

class Typestate(filename:String) extends scala.annotation.StaticAnnotation


object doMainThings{
  def main(args: Array[String]): Unit = {
    var cat = new Cat()
    var cat1 = new Cat()
    var x =1
    val cat2 = if(x==1) cat else cat1
  }

  def createCat(): Unit ={
    createCat()
  }

  /*
  //@Typestate(filename="src\\main\\scala\\ProtocolDSL\\DogProtocol.scala")
  object Dog extends Serializable{
    def walk():Unit = println("Jee kävelemme!")
    def cry():Unit = println("Itken :'(")
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
  */

}

@Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
case class Cat(var id:Int=0){
  println("init "+id)
  def selfChange(kit:Cat): Unit ={
    kit.walk()
  }
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




