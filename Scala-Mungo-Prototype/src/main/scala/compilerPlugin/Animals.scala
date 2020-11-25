package compilerPlugin

import scala.collection.mutable

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
  var cat:Cat = null
  println("printing")
}

object Main extends App{
  var cat:Cat = new Cat
  cat.friend = new Cat


}



