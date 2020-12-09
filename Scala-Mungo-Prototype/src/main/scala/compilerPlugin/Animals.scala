package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation

/*
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

 */

@Typestate("MoneyStashProtocol")
class MoneyStash() {
  var amountOfMoney : Float = 0

  def fill(amount : Float ) : Unit = {
    amountOfMoney = amount
  }

  def get() : Float = amountOfMoney


  def applyInterest(interest_rate : Float) : Unit = {
    amountOfMoney = amountOfMoney * interest_rate;
  }
}

class DataStorage() {
  var money : MoneyStash = null;

  def setMoney(m : MoneyStash) : Unit = {
    money = m
  }

  def store() : Unit = {
    var amount = money.get()
    println(amount)
    // write to DB
  }
}

class SalaryManager() {
  var money : MoneyStash = null;

  def setMoney(m : MoneyStash) : Unit = {
    money = m
  }

  def addSalary(amount: Float) : Unit = {
    money.fill(amount)
    money.applyInterest(1.02f) // <- if this line is omitted,
    //    then an error should be thrown
  }
}


object Demonstration extends App {
  val salary = new MoneyStash
  val manager = new SalaryManager
  val storage = new DataStorage

  manager.setMoney(salary)
  storage.setMoney(salary)

  manager.addSalary(5000)
  storage.store()
}




