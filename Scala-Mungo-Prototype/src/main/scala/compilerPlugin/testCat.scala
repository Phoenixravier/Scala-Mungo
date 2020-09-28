/*
package compilerPlugin


class Typestate(filename:String) extends scala.annotation.StaticAnnotation


@Typestate(filename = "src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
class Cat{
  def comeAlive(): Unit = println("The cat is alive")
  def walk(): Boolean = true
}

object doMain extends App{
  val cat = new Cat()
  var cat1 = new Cat()
  var x = 1
  var cat2 = if(x==1) cat else cat1
  cat.walk()
  cat1.walk()
  cat2.walk()
}
*/
