package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation
@Typestate("CatProtocol")
class Cat{
  var friend:Cat = null
  var age = 0
  var container:CatContainer = _
  def walk(): Boolean = true
  def walkFriend(): Unit ={
    Cat.this.friend.walk()
  }
  def setFriend(f:Cat): Unit ={
    friend = f
  }
  def incrFriendAge(): Unit ={
    friend.age += 1
  }
}
class CatContainer(){
  var cat:Cat=null
  def createCat(cat:Cat): Unit ={
    var cat = new Cat()
  }
}


object Main{
  val fox = Cat
  def main(args: Array[String]): Unit = {
    Cat.walk()
    Cat.walk()
  }
}

@Typestate("CatProtocol")
object Cat{
  def walk(): Boolean = true
  def walkFriend(): Unit ={
    walk()
  }
  def setFriend(f:Cat): Unit ={
  }
}




