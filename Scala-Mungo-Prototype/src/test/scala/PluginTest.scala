import java.io.{BufferedWriter, ByteArrayOutputStream, File, FileWriter}

import ProtocolDSL.State
import compilerPlugin.{GetFileFromAnnotation, protocolViolatedException}
import compilerPlugin.MyComponent
import ProtocolDSL.{ReturnValue, State}

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{Global, Settings}
import org.scalatest._

import scala.collection.SortedSet


class PluginTest extends FlatSpec with Matchers {

  "plugin" should "only recognise the typestate annotation" in {
    val felineProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State1"
        |    in ("State1")
        |    end()
        |  }
        |}
        |""".stripMargin
    writeFile("FelineProtocol.scala", Seq(felineProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |class Nonsense(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Nonsense(filename = "FelineProtocol.scala")
        |class Trash{
        | def comeAlive():Unit = println("The trash awakens")
        | def walk():Unit = println("walking")
        |}
        |
        |
        |@Typestate(filename = "FelineProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val t = new Trash()
        |  t.walk()
        |  t.walk()
        |  val cat = new Cat()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "throw an exception when an invalid transition happens in a class" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end
        |  }
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  cat.comeAlive()
        |  cat.walk()
        |  cat.comeAlive()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val exceptedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "comeAlive()", "<test>", 32)
    assert(actualException.getMessage == exceptedException.getMessage)
   }

  "plugin" should "throw an exception when an invalid transition happens in an object" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end
        |  }
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |object Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  Cat.comeAlive()
        |  Cat.walk()
        |  Cat.comeAlive()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val exceptedException = new protocolViolatedException("Cat", "Cat",
      sortSet(Set(State("State3", 1))), "comeAlive()", "<test>", 31)
    assert(actualException.getMessage === exceptedException.getMessage)
  }

  "plugin" should "throw an exception when an invalid transition happens in a class in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end
        |  }
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  cat.comeAlive()
        |  cat.walk()
        |  cat.comeAlive()
        |}
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "comeAlive()", "<test>", 33)
    assert(actualException.getMessage === expectedException.getMessage)
   }

  "plugin" should "throw an exception when an invalid transition happens in an object in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end
        |  }
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |object Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  Cat.comeAlive()
        |  Cat.walk()
        |  Cat.comeAlive()
        |}
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("Cat", "Cat",
      sortSet(Set(State("State3", 1))), "comeAlive()", "<test>", 32)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception when protocol methods are not a subset of ones in class" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    when ("notAMethod()") goto "State3"
        |    in ("State2")
        |    in ("State1")
        |    end
        |  }
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  cat.comeAlive()
        |  cat.walk()
        |  cat.comeAlive()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(actualException.getMessage ===
      "Methods Set(notAMethod(), walk(), comeAlive(), die()) defined in \"MyProtocol.scala\" are not a subset of " +
        "methods Set(comeAlive(), walk(), die()) defined in class Cat")
  }

  "plugin" should "throw an exception if end is not written in the protocol" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    when ("notAMethod()") goto "State3"
        |    in ("State2")
        |    in ("State1")
        |  }
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  cat.comeAlive()
        |  cat.walk()
        |  cat.comeAlive()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(actualException.getMessage ===
      "The protocol at \"MyProtocol.scala\" could not be processed, " +
        "check you have an end statement at the end of the protocol")
  }

  "plugin" should "deal with multiple classes with protocols" in {
    val catProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang{
        |  def main(args:Array[String]) = {
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    when ("die()") goto
        |      "State1" at "True" or
        |      "State2" at "False" or
        |      "State3" at "Maybe" or
        |      "State1" at null
        |
        |    in ("State3")
        |    when ("notAMethod()") goto "State3"
        |    in ("State2")
        |    in ("State1")
        |    end()
        |  }
        |}
        |""".stripMargin
    writeFile("CatProtocol.scala", Seq(catProtocol))
    val dogProtocol =
      """
        |package ProtocolDSL
        |
        |object DogProtocol extends ProtocolLang with App{
        |  in ("init")
        |  when("walk():Unit") goto "walking"
        |  when("cry():Unit") goto "crying"
        |  when("laze():Unit") goto "lazing"
        |  when("stayOnAlert(Boolean):Unit") goto "onAlert"
        |
        |  in ("walking")
        |  when("bark():Unit") goto "walking"
        |  when("cry():Unit") goto "crying"
        |
        |  in("crying")
        |  when("laze():Unit") goto "lazing"
        |
        |  in("lazing")
        |  when("stayOnAlert(Boolean):Unit") goto "onAlert"
        |
        |  in("onAlert")
        |  when("bark():Unit") goto "onAlert"
        |  when("laze():Unit") goto "lazing"
        |  end()
        |
        |}
        |""".stripMargin
    writeFile("DogProtocol.scala", Seq(dogProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "CatProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |@Typestate(filename="DogProtocol.scala")
        |  class Dog extends Serializable{
        |    def walk():Unit = println("Yee kavelemme!")
        |    def cry():Unit = println("Itkeen :'(")
        |    def bark():Unit = println("hau hau")
        |    def laze():Unit = println("Olen vasinyt")
        |    def stayOnAlert(intruderHere:Boolean): Unit = {
        |      if(intruderHere) bark()
        |      else laze()
        |    }
        |    def stayOnAlert(str:String, nb:Int): Unit ={
        |      println("on alert")
        |    }
        |  }
        |
        |object Main extends App {
        |  val dog = new Dog()
        |  dog.walk()
        |  dog.walk()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("CatProtocol.scala")
    deleteFile("DogProtocol.scala")
    assert(actualException.getMessage === "Methods Set(notAMethod(), walk(), comeAlive(), die()) defined in \"CatProtocol.scala\" are not a subset of methods Set(comeAlive(), walk(), die()) defined in class Cat")
  }

  /* -----------------
         FOR LOOPS
   -------------------*/

  "plugin" should "throw an exception if an instance violates its protocol inside a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- 1 to 10) cat.walk()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 30)
    assert(actualException.getMessage === expectedException.getMessage)
 }

  "plugin" should "throw an exception if an instance defined inside a for loop violates its protocol" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- 1 to 10) {
        |   val kitty = new Cat()
        |   kitty.comeAlive()
        |   kitty.walk()
        |   kitty.walk()
        |  }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 34)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance after being in a valid for loop violates its protocol" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    when("walk()") goto "State2"
        |    in ("State2")
        |    when("walk()") goto "init"
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |sealed trait DeathState
        |case object Dead extends DeathState
        |case object Alive extends DeathState
        |case object Unsure extends DeathState
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def die():DeathState = {
        |    val randomGenerator = Random
        |    val randomNumber = randomGenerator.nextDouble()
        |    println(randomNumber)
        |    if(randomNumber < 0.25) Dead
        |    else if(randomNumber < 0.5) Alive
        |    else if(randomNumber < 0.75) Unsure
        |    else null
        |  }
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- 1 to 10) {
        |   val kitty = new Cat()
        |   kitty.comeAlive()
        |   kitty.walk()
        |   cat.walk()
        |  }
        |  cat.comeAlive()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1), State("State2", 2), State("init",0))), "comeAlive()", "<test>", 36)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "not throw an exception if two instances with looping protocols of different lengths are in a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State1"
        |    in ("State1")
        |    when("comeAlive()") goto "init"
        |    when("run()") goto "State2"
        |    in ("State2")
        |    when("run()") goto "State3"
        |    in ("State3")
        |    when("run()") goto "init"
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def run(): Unit = println("running")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  val kitty = new Cat()
        |  for(i <- 1 to 10) {
        |  cat.walk()
        |  cat.comeAlive()
        |  kitty.walk()
        |  kitty.run()
        |  kitty.run()
        |  kitty.run()
        |  }
        |
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
  }

  /* -----------------
     FOR LOOP GENERATORS
   -------------------*/

  "plugin" should "throw an exception if an instance violates its protocol in the to generator of a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getBirthAge(cat) to getCatAge(cat)) cat.walk()
        |
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 20)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the to generator of a for loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  for(i <- getBirthAge(cat) to getCatAge(cat)) cat.walk()
        |}
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the until generator of a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getBirthAge(cat) until getCatAge(cat)) cat.walk()
        |
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 20)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the until generator of a for loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  for(i <- getBirthAge(cat) until getCatAge(cat)) cat.walk()
        |}
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat)) println("for")
        |
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 26)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat)) println("for")
        |}
        |
        |   def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the multiple generators of a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for{
        |  i <- getCatAgeRange(cat)
        |  j <- getKittyAgeRange(cat)
        |  } println("for")
        |
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |  def getKittyAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,4)
        |  }
        |
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 24)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the multiple generators  of a for loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  for{
        |  i <- getCatAgeRange(cat)
        |  j <- getKittyAgeRange(cat)
        |  } println("for")
        | }
        |
        |   def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |  def getKittyAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,4)
        |  }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 26)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with guard of a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Int = 1
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat) if (i == cat.walk())) println("for")
        |
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 26)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with guard of a for loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Int = 1
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat) if (i == cat.walk())) println("for")
        |}
        |
        |   def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with multiple guards of a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Int = 1
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat) if (i == 0) if(cat.walk() == 0)) println("for")
        |
        |   def getCatAge(cat:Cat): Int ={
        |    println("inside get cat age")
        |    cat.walk()
        |    10
        |  }
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |
        |  def getBirthAge(kitty: Cat) = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    0
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 26)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with multiple guards of a for loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Int = 1
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat) if (i == 0) if(cat.walk() == 0)) println("for")
        |}
        |
        |   def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
        |  }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for yield loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  val ones = for(i <- getBirthAgeRange(cat)) yield 1
        |
        |
        |  def getBirthAgeRange(kitty: Cat):List[Int] = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    List(0,10)
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 21)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for yield loop in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val ones = for(i <- getBirthAgeRange(cat)) yield 1
        |
        |}
        |  def getBirthAgeRange(kitty: Cat):List[Int] = {
        |    println("inside get birth age")
        |    kitty.walk()
        |    List(0,10)
        |  }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  /* -----------------
         WHILE LOOPS
   -------------------*/

  "plugin" should "throw an exception if an instance after being in a valid while loop violates its protocol" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    when("walk()") goto "State2"
        |    in ("State2")
        |    when("walk()") goto "init"
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  var x=0
        |  while(x<10) {
        |   val kitty = new Cat()
        |   kitty.comeAlive()
        |   kitty.walk()
        |   cat.walk()
        |   x+=1
        |  }
        |  cat.comeAlive()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1), State("State2", 2), State("init",0))), "comeAlive()", "<test>", 25)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception with the correct states if an instance after being in a valid do-while loop violates its protocol" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    when("walk()") goto "State2"
        |    in ("State2")
        |    when("walk()") goto "State3"
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  var x=0
        |   do {
        |   cat.walk()
        |   x+=1
        |  } while(x<10)
        |  cat.comeAlive()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1), State("State2", 2))), "comeAlive()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if the condition inside the do while causes a protocol violation" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    when("walk()") goto "init"
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  var x=0
        |   do {
        |   cat.comeAlive()
        |  } while(makeCatWalk(cat))
        |
        |  def makeCatWalk(cat:Cat): Boolean={
        |   cat.walk()
        |   true
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("init", 0))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if the condition inside a while causes a protocol violation" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    when("walk()") goto "init"
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  var x=0
        |   while(makeCatWalk(cat)) {
        |   cat.comeAlive()
        |  }
        |
        |  def makeCatWalk(cat:Cat): Boolean={
        |   cat.walk()
        |   true
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("init", 0))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "not throw an exception if a protocol violation happens after a while(true) loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State1"
        |    in ("State1")
        |    when("comeAlive()") goto "init"
        |    when("run()") goto "State2"
        |    in ("State2")
        |    when("run()") goto "State3"
        |    in ("State3")
        |    when("run()") goto "init"
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |  def run(): Unit = println("running")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  val kitty = new Cat()
        |  while(true){
        |  cat.walk()
        |  cat.comeAlive()
        |  }
        |  kitty.walk()
        |  kitty.walk()
        |
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
  }

  /* -----------------
         MIXED LOOPS
   -------------------*/

  "plugin" should "throw an exception when inner loop causes a violation" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State1"
        |    when("comeAlive()") goto "init"
        |    in ("State1")
        |    when ("comeAlive()") goto "State2"
        |    in ("State2")
        |    when("walk()") goto "init"
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  var x=0
        |   do {
        |   cat.walk()
        |   for(y <- 1 to 100) cat.comeAlive()
        |   cat.walk()
        |   x+=1
        |  } while(x<10)
        |  cat.comeAlive()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State2", 1))), "comeAlive()", "<test>", 20)
    assert(actualException.getMessage === expectedException.getMessage)
  }



  /* -----------------
         FUNCTIONS
   -------------------*/

  "plugin" should "throw an exception if an instance violates its protocol inside a function" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  makeCatWalk(cat)
        |  makeCatWalk(cat)
        |
        |  def makeCatWalk(kitty:Cat): Unit ={
        |      kitty.walk()
        |    }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 21)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function with one parameter" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  takeInt(makeCatWalk(cat))
        |  takeInt(makeCatWalk(cat))
        |
        |
        | def takeInt(int:Int): Unit={
        | }
        |
        |  def makeCatWalk(kitty:Cat): Int ={
        |      kitty.walk()
        |      1
        |    }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 25)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function with multiple parameters" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  takeInts(makeCatWalk(cat), makeCatWalk(cat))
        |
        |
        | def takeInts(int:Int, int2:Int): Unit={
        | }
        |
        |  def makeCatWalk(kitty:Cat): Int ={
        |      kitty.walk()
        |      1
        |    }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 24)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a nested function" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  makeCatWalk(cat)
        |
        |  def makeCatWalk(kitty:Cat): Unit ={
        |      kitty.walk()
        |      makeCatComeAlive(kitty)
        |      def makeCatComeAlive(cat:Cat): Unit ={
        |         cat.comeAlive()
        |      }
        |   }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "comeAlive()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside two nested functions" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  makeCatComeAlive(cat)
        |
        |  def makeCatComeAlive(kitty:Cat): Unit ={
        |      kitty.comeAlive()
        |      makeCatComeAliveAgain(kitty)
        |      def makeCatComeAliveAgain(cat:Cat): Unit ={
        |         cat.comeAlive()
        |         makeCatWalkTwice(cat)
        |         def makeCatWalkTwice(cat:Cat): Unit ={
        |             cat.walk()
        |             cat.walk()
        |         }
        |      }
        |   }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 27)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  makeCatWalk(cat)
        |  makeCatWalk(cat)
        |
        |  def makeCatWalk(kitty:Cat): Unit ={
        |      kitty.walk()
        |    }
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a nested function in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  makeCatWalk(cat)
        |
        |  def makeCatWalk(kitty:Cat): Unit ={
        |      kitty.walk()
        |      makeCatComeAlive(kitty)
        |      def makeCatComeAlive(cat:Cat): Unit ={
        |         cat.comeAlive()
        |      }
        |   }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "comeAlive()", "<test>", 24)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside two nested functions in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  makeCatComeAlive(cat)
        |
        |  def makeCatComeAlive(kitty:Cat): Unit ={
        |      kitty.comeAlive()
        |      makeCatComeAliveAgain(kitty)
        |      def makeCatComeAliveAgain(cat:Cat): Unit ={
        |         cat.comeAlive()
        |         makeCatWalkTwice(cat)
        |         def makeCatWalkTwice(cat:Cat): Unit ={
        |             cat.walk()
        |             cat.walk()
        |         }
        |      }
        |   }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 28)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an outer functions called by an inner one in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  makeCatComeAlive(cat)
        |
        |  def makeCatComeAlive(kitty:Cat): Unit ={
        |      val cat = new Cat()
        |    makeCatWalk(cat)
        |    def makeCatWalk(cat:Cat): Unit ={
        |      makeWalk(cat)
        |      def makeWalk(cat:Cat): Unit ={
        |        walk(cat)
        |        walk(cat)
        |      }
        |    }
        |
        |    def walk(cat:Cat): Unit ={
        |      cat.walk()
        |    }
        |   }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 32)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance defined in a function violates its protocol inside a function" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  makeCatWalkTwice()
        |
        |  def makeCatWalkTwice(): Unit ={
        |      val cat = new Cat()
        |      cat.walk()
        |      cat.walk()
        |    }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 21)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance defined in a function violates its protocol inside a function in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  makeCatWalkTwice()
        |
        |  def makeCatWalkTwice(): Unit ={
        |      val cat = new Cat()
        |      cat.walk()
        |      cat.walk()
        |    }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State3", 1))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "not throw an exception if two instances with the same name do one legal method each" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto "State3"
        |    when ("comeAlive()") goto "init"
        |
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        | def makeCatWalk(): Unit ={
        |      val cat = new Cat()
        |      cat.walk()
        |  }
        |  makeCatWalk()
        |  val cat = new Cat()
        |  cat.walk()
        |  }
        |}
        |
        |""".stripMargin

    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
  }

  /* -----------------
       RETURN VALUES
   -------------------*/

  "plugin" should "throw an exception if an instance could violate its protocol after a method giving multiple states in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 19)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance could violate its protocol after a method giving multiple states" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |   val cat = new Cat()
        |   cat.walk()
        |   cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 18)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  /* -----------------
         IF ELSE
   -------------------*/

  "plugin" should "throw an exception if an instance could violate its protocol after an if else statement in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |    var x = 1
        |    if(x == 1)
        |     cat.walk()
        |    else
        |     cat.walk()
        |    cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance could violate its protocol after an if else statement" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val cat = new Cat()
        |    var x = 1
        |    if(x == 1)
        |     cat.walk()
        |    else
        |     cat.walk()
        |    cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an if else statement" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val cat = new Cat()
        |    var x = 1
        |    if(x == 1){
        |     val kitty = new Cat()
        |     kitty.walk()
        |     kitty.walk()
        |     cat.walk()
        |    }
        |    else
        |     cat.walk()
        |    cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 21)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an if else statement in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |    var x = 1
        |    if(x == 1){
        |     val kitty = new Cat()
        |     kitty.walk()
        |     kitty.walk()
        |     cat.walk()
        |     }
        |    else
        |     cat.walk()
        |    cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an if else statement inside a for loop" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |    for(y <- 1 to 10){
          |    var x = 1
          |    if(x == 1){
          |     val kitty = new Cat()
          |     kitty.walk()
          |     kitty.walk()
          |     cat.walk()
          |     }
          |    else
          |     cat.walk()
          |    cat.walk()
        |    }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("kitty", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol after a singular if statement" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val cat = new Cat()
        |    var x = 1
        |    if(x == 1){
        |     cat.walk()
        |    }
        |    cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 21)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the condition of the if in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |    var x = 1
        |    if(cat.walk() && cat.walk()) println("if")
        |    else println("else")
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 19)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the condition of the if" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val cat = new Cat()
        |    var x = 1
        |    if(cat.walk() && cat.walk()) println("if")
        |    else println("else")
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 18)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the  complex condition of the if in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |    var x = 1
        |    if(cat.walk() && (false || (cat.walk() && true))) println("if")
        |    else println("else")
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 19)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the complex condition of the if" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val cat = new Cat()
        |    var x = 1
        |    if(cat.walk() && (false || (cat.walk() && true))) println("if")
        |    else println("else")
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 18)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  /* ------------------------------
     CODE IN CLASSES AND OBJECTS
   --------------------------------*/

  "plugin" should "throw an exception if an instance violates its protocol inside a class construtor" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val kat = new Cat()
        |    new CatMaker(kat)
        |}
        |
        |class CatMaker(cat:Cat){
        |  cat.walk()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a class construtor in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val kat = new Cat()
        |    new CatMaker(kat)
        |  }
        |  class CatMaker(cat:Cat){
        |  cat.walk()
        |  cat.walk()
        |}
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object constructor" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    Dog.walk()
        |
        |    object Dog extends Serializable{
        |    println("made a dog")
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |    def walk():Unit = println("Jee kävelemme!")
        |    def cry():Unit = println("Itkeen :'(")
        |    def bark():Unit = println("hau hau")
        |    def laze():Unit = println("Olen vasinyt")
        |    def stayOnAlert(intruderHere:Boolean): Unit = {
        |      if(intruderHere) bark()
        |      else laze()
        |    }
        |    def stayOnAlert(str:String, nb:Int): Unit ={
        |      println("on alert")
        |    }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object construtor in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    Dog.walk()
        |
        |    object Dog extends Serializable{
        |    println("made a dog")
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |    def walk():Unit = println("Jee kävelemme!")
        |    def cry():Unit = println("Itkeen :'(")
        |    def bark():Unit = println("hau hau")
        |    def laze():Unit = println("Olen vasinyt")
        |    def stayOnAlert(intruderHere:Boolean): Unit = {
        |      if(intruderHere) bark()
        |      else laze()
        |    }
        |    def stayOnAlert(str:String, nb:Int): Unit ={
        |      println("on alert")
        |    }
        |   }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object construtor, alone" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    Dog
        |
        |    object Dog extends Serializable{
        |    println("made a dog")
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |    def walk():Unit = println("Jee kävelemme!")
        |    def cry():Unit = println("Itkeen :'(")
        |    def bark():Unit = println("hau hau")
        |    def laze():Unit = println("Olen vasinyt")
        |    def stayOnAlert(intruderHere:Boolean): Unit = {
        |      if(intruderHere) bark()
        |      else laze()
        |    }
        |    def stayOnAlert(str:String, nb:Int): Unit ={
        |      println("on alert")
        |    }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object construtor, alone in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    Dog
        |
        |    object Dog extends Serializable{
        |    println("made a dog")
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |    def walk():Unit = println("Jee kävelemme!")
        |    def cry():Unit = println("Itkeen :'(")
        |    def bark():Unit = println("hau hau")
        |    def laze():Unit = println("Olen vasinyt")
        |    def stayOnAlert(intruderHere:Boolean): Unit = {
        |      if(intruderHere) bark()
        |      else laze()
        |    }
        |    def stayOnAlert(str:String, nb:Int): Unit ={
        |      println("on alert")
        |    }
        |   }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 23)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object containing main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | val cat = new Cat
        | cat.walk()
        | cat.walk()
        |
        | def main(args: Array[String]): Unit = {
        | }
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    val expectedException = new protocolViolatedException("cat", "Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 18)
    assert(actualException.getMessage === expectedException.getMessage)
  }

  /* ------------------------------
          TRY CATCH FINALLY
   --------------------------------*/

  "plugin" should "not throw an exception if an instance doesn't violate its protocol inside a try catch" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val cat = new Cat()
        |     try{
        |     cat.walk()
        |    }
        |    catch{
        |     case e: FileNotFoundException => cat.walk()
        |    }
        |    finally{
        |    }
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
  }

  "plugin" should "not throw an exception if an instance doesn't violate its protocol inside a try catch in main" in {
    val userProtocol =
      """
        |package ProtocolDSL
        |
        |object Example extends ProtocolLang with App{
        |    in ("init")
        |    when ("walk()") goto
        |      "State1" at "true" or
        |      "init" at "false"
        |    when ("comeAlive()") goto "init"
        |    in ("State3")
        |    in ("State2")
        |    in ("State1")
        |    end()
        |}
        |""".stripMargin
    writeFile("MyProtocol.scala", Seq(userProtocol))
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "MyProtocol.scala")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val cat = new Cat()
        |     try{
        |     cat.walk()
        |    }
        |    catch{
        |     case e: FileNotFoundException => cat.walk()
        |    }
        |    finally{
        |    }
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
  }





  def createCompiler(code:String): (Global, List[BatchSourceFile]) ={
    val sources = List(new BatchSourceFile("<test>", code))
    val settings = new Settings
    settings.usejavacp.value = true
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    val compiler = new Global(settings, new ConsoleReporter(settings)) {
      override protected def computeInternalPhases () {
        super.computeInternalPhases
        for (phase <- new GetFileFromAnnotation(this).components)
          phasesSet += phase
      }
    }
    (compiler, sources)
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line.trim())
    }
    bw.close()
  }

  private def deleteFile(path: String) = {
    val fileTemp = new File(path)
    if (fileTemp.exists) {
      fileTemp.delete()
    }
  }

  /** Sorts a set */
  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] = SortedSet.empty[A] ++ unsortedSet

}

