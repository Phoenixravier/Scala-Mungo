package compilerPlugin

import scala.collection.mutable

class Typestate(filename: String) extends scala.annotation.StaticAnnotation
@Typestate("CatProtocol")
class Cat{
  var friend:Cat = null
  var friend4:Cat = _
  var age = 0
  var container:CatContainer = _
  def walk(): Boolean = true
  def walkFriend(): Unit ={
    Cat.this.friend.walk()
  }
  def setFriend4(f:Cat): Unit ={
    friend4 = f
  }
  def setFriend(f:Cat): Unit ={
    friend = f
  }
  def incrFriendAge(): Unit ={
    friend.age += 1
  }
}
class CatContainer{
  var cat:Cat=null
  def dash(): Unit ={

  }
}

object Printer{
  var cat:Cat = null
  println("printing")
}

object Main extends App{
  var cat:Cat = new Cat
  var cat2 = new Cat
  cat.setFriend(cat2)
  cat.walkFriend()
  cat2.walk()
}



