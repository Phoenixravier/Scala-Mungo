package compilerPlugin


class Typestate(filename:String) extends scala.annotation.StaticAnnotation

class Mammal(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filenam="src\\main\\scala\\MyProtocol.txt")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def run(): Unit = println("Running")
  def rest(): Unit = println("Resting")
  def walk(): Unit = println("Walking")
  def sleep(): Unit = println("Sleeping")
  def die(): Unit = println("The cat is dead")
}

@Typestate(filename="Lol.txt")
class Doggo{
  def sleep(): Unit = println("shleep")
}

object Main extends App {
  val cat = new Cat()
  cat.comeAlive()
  cat.run()

}
