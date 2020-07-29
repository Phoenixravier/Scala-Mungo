package compilerPlugin

class Mammal(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename="src\\main\\scala\\MyProtocol.txt")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def run(): Unit = println("Running")
  def rest(): Unit = println("Resting")
  def walk(): Unit = println("Walking")
  def sleep(): Unit = println("Sleeping")
  def die(): Boolean = {
    println("The cat is dead")
    true
  }
}

@Typestate(filename="Lol.txt")
class Doggo{
  def sleep(): Unit = println("shleep")
}

object Main extends App {
  System.getProperty("java.class.path")
  val cat = new Cat()
  cat.comeAlive()
  cat.run()

}
