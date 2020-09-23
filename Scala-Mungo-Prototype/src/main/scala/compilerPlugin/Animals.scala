package compilerPlugin


import java.io.{FileNotFoundException, FileOutputStream, IOException, ObjectOutputStream}


import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

class Typestate(filename:String) extends scala.annotation.StaticAnnotation


object doMainThings extends App{
  val cat = new Cat()
  var cat1 = new Cat()
  var cat2 = new Cat()
  cat1 = cat
  cat1.walk()
  cat2 = cat
  cat2.walk()


  def createCat(): Cat ={
    val kitty = new Cat(1)
    kitty.walk()
    kitty
  }


  def getCatAge(cat:Cat): Int ={
    println("inside get cat age")
    cat.walk()
    10
  }

  def getCatAgeRange(cat:Cat): List[Int] ={
    println("inside get cat age range")
    cat.walk()
    List(0,10)
  }

  def getBirthAge(kitty: Cat) = {
    println("inside get birth age")
    kitty.walk()
    0
  }

  def returnTrue():Boolean ={
    val cat = new Cat(1)
    cat.walk()
    cat.walk()
    true
  }
  def makeCatWalk(set:Cat): Unit ={
    set.walk()
    set.walk()
  }

  //@Typestate(filename="src\\main\\scala\\ProtocolDSL\\DogProtocol.scala")
  object Dog extends Serializable{
    println("made a dog")
    val cat = new Cat(1)
    cat.walk()
    cat.walk()
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

}

object Trash{
    def someMethod(): Unit ={
    }
  println("inside object trash")
}


@Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
case class Cat(var id:Int=0){
  println("init "+id)

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




