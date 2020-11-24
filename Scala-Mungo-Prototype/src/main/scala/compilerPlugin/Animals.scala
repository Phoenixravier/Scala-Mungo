package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation
@Typestate("CatProtocol")
class Cat{
  var friend:Cat = null
  var friend4:Cat = _
  var age = 0
  def walk(): Boolean = true
  def walkFriend(): Unit ={
    friend.walk()
  }
  def setFriend4(f:Cat): Unit ={
    friend4 = f
    friend4.friend = new Cat()
  }
  def incrFriendAge(): Unit ={
    friend.age += 1
  }
}
object Printer{
  println("printing")
}

object Main extends App{
  val cat = new Cat()
  var cat2 = new Cat()
  cat.friend = cat2
  val cat3 = cat.friend
  cat.incrFriendAge()
  println(cat.age)
  println(cat.friend.age)
  println(cat2.age)
  println(cat3.age)

}



