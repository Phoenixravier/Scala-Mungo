package compilerPlugin

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

@Typestate("MoneyStashProtocol")
class MoneyStash() {
  var amountOfMoney : Float = 0
  def fill(amount : Float ) : Unit = {amountOfMoney = amount}
  def get() : Float = amountOfMoney
  def applyInterest(interest_rate : Float) : Unit = {
    amountOfMoney = amountOfMoney * interest_rate;
  }
}

class DataStorage() {
  var money : MoneyStash = null;
  def setMoney(m : MoneyStash) : Unit = {money = m}
  def store() : Unit = {
    var amount = money.get()
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
    money.applyInterest(1.02f)
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
  storage.store()
}


