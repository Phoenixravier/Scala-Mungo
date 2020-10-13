package compilerPlugin




class Typestate(filename:String) extends scala.annotation.StaticAnnotation


@Typestate(filename = "src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
class Cat{

  println("making a cat")
  var age:Int=_
  def comeAlive(): Unit = println("The cat is alive")
  def walk(): Boolean = {
    println("in walking function")
    true
  }
}

object Cat{
  def apply(): Cat = {
    println("making a cat in object")
    new Cat()
  }

  def walk(cat:Cat): Unit ={
    println("doing sthg")
    cat.walk()
    cat.walk()
  }

}

object Main extends App{
  val cat = Cat()
  Cat.walk(cat)
  def apply(): Unit ={
    val kitty = new Cat()
    kitty.walk()
    kitty.walk()
  }
}




//import scala.language.postfixOps
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
*/





