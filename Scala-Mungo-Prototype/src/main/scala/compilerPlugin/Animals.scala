package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation
@Typestate(filename = "walkTwiceIllegalProtocol")
class Cat{
  var friend:Cat = null
  def walk(): Boolean = true
  def walkFriend(): Unit ={
    friend.walk()
  }
  def setFriend(f:Cat): Unit ={
    friend = f
  }
}

object Cat{
  var kitty = new Cat()
}

object Main extends App{
  val cat1 = new Cat()
  val cat2 = new Cat()
  cat1.setFriend(cat2)
  cat1.walkFriend()
  cat2.walk()
  Cat.kitty
}





