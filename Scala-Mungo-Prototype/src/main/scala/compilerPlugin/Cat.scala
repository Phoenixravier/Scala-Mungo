package compilerPlugin


class Typestate(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename = "MyProtocol.txt")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def run(): Unit = println("Running")
  def rest(): Unit = println("Resting")
  def walk(): Unit = println("Walking")
  def sleep(): Unit = println("Sleeping")
  def die(): Unit = println("The cat is dead")
}

object Main extends App {
  val cat = new Cat()
  cat.comeAlive()
  cat.run()

}
