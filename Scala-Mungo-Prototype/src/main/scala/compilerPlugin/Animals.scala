package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation
@Typestate("CatProtocol")
class Cat{
  var friend:Cat = null
  var friend4:Cat = _
  def walk(): Boolean = true
  def walkFriend(): Unit ={
    friend.walk()
  }
  def setFriend(f:Cat): Unit ={
    friend4 = f
  }
}

object Main extends App{
  var cat1:Cat = _

  def getNull():Cat = null

  var cat2:Cat = getNull()
  println(cat2)

}

