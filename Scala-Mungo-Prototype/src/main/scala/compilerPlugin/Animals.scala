package compilerPlugin

//import Cats.{Cat => Cat}
//import Dogs.{Cat => Dog}

class Typestate(filename: String) extends scala.annotation.StaticAnnotation

@Typestate("MoneyStashProtocol")
class MoneyStash() {
  var amountOfMoney = 0

  def fill(): Unit ={
    println("filling")
    amountOfMoney = 10
  }

  def get(): Unit ={
    println("getting")
    amountOfMoney -= 10
  }
}

object Demonstration extends App{
  def fillStash(stash:MoneyStash): Int ={
    stash.fill()
    0
  }
  def getStash(stash:MoneyStash): Int ={
    stash.get()
    10
  }
  val stash = new MoneyStash
  for(x <- fillStash(stash) to getStash(stash)){
    println("iteration "+x)
  }
}

