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

object Demonstration extends App{
  val stash = new MoneyStash
  val sameStash = stash
  stash.fill()
  sameStash.get()
  stash.get()
}

