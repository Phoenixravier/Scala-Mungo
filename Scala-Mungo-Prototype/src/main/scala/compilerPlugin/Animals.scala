package compilerPlugin

//import Cats.{Cat => Cat}
//import Dogs.{Cat => Dog}

class Typestate(filename: String) extends scala.annotation.StaticAnnotation

@Typestate("MoneyStashProtocol")
class MoneyStash() {
  var amountOfMoney = 0

  def fill(): Unit ={
    amountOfMoney = 10
  }

  def get(): Unit ={
    amountOfMoney -= 10
  }

}

@Typestate("CWeirdProtocol")
class Cat{
  var friend:Cat = null
  var friendToo:Cat = new Cat()
  var friend3 = new Cat()
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
  val cat1 = new Cat()
  val cat2 = new Cat()
  cat1.friend = cat2
  cat1.setFriend(cat2)
  cat1.walkFriend()
  cat2.walk()
}

