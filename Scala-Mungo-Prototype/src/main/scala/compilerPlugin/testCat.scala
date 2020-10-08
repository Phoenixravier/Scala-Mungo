/*
package compilerPlugin


class Typestate(filename:String) extends scala.annotation.StaticAnnotation
class Nonsense(filename:String) extends scala.annotation.StaticAnnotation

@Nonsense(filename = "src\\main\\scala\\ProtocolDSL\\SomeProtocol.scala")
class Trash{
 def comeAlive():Unit = println("The trash awakens")
 def walk():Unit = println("walking")
}


@Typestate(filename = "src\\main\\scala\\ProtocolDSL\\SomeProtocol.scala")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def walk(): Unit = println("walking")
}

object Main extends App {
  val t = new Trash()
  t.walk()
  t.walk()
  val cat = new Cat()
  cat.walk()
}

 */
