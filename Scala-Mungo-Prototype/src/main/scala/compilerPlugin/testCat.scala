class Typestate(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename = "src\\main\\scala\\ProtocolDSL\\SomeProtocol.scala")
class testCat{
  def comeAlive(): Unit = println("The cat is alive")
  def walk(): Unit = println("walking")
}

object Main extends App {
  val cat = new testCat()
  cat.comeAlive()
  cat.walk()
  cat.comeAlive()
}
