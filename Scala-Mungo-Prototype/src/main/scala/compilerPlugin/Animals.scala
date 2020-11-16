package compilerPlugin

//import Cats.{Cat => Cat}
//import Dogs.{Cat => Dog}

class Typestate(filename: String) extends scala.annotation.StaticAnnotation

@Typestate("CatProtocol")
class Cat(var name: String) {
  var friend: Cat = _
  var number = 0

  def walk() = {
    println(name + " is walking")
  }

  def walkWithFriend() = {
    //this.walk()
    friend.walk()
    friend.number =4
  }

  def setFriend(f : Cat) = {
    friend = f

  }
}

object Demonstration {
  def main(args: Array[String]) = {
    val c1 = new Cat("Charlie")
    val c2 = new Cat("Tiger")
    c1.setFriend(c2)
    c1.walkWithFriend()
    c2.walk()
    println(c2.number)
  }
}

