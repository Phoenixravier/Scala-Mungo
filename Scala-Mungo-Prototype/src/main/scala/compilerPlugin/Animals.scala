package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation

/*
@Typestate("CatProtocol")
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


object Main{
  val fox = Cat

  def main(args: Array[String]): Unit = {
    Cat.walk()
    Cat.walk()
  }

}

object Main extends App{
  val cat1 = new Cat()
  val cat2 = new Cat()
  cat1.setFriend(cat2)
  cat1.walkFriend()
  cat2.walk()
  Cat.kitty
}

 */
/*
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

  storage.store()
  manager.addSalary(5000)

}
*/

@Typestate("CatProtocol")
class Cat(var name:String) {
  var friend: Cat = _
  var number = 0

  def walk(): Boolean = {
    println(name + " is walking")
    true
  }

  def walkWithFriend() = {
  }

  def setFriend(f : Cat) = {
    friend = f
  }
}

object oravaUsed extends App{
  val cat = new Cat("Alice")
  cat.walk() match{
    case true =>
    case false =>
      cat.walkWithFriend()
  }
}




