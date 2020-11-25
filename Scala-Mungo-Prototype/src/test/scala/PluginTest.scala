import java.io.{BufferedWriter, File, FileWriter}

import ProtocolDSL.State
import compilerPlugin.{GetFileFromAnnotation, inconsistentStateMutation, protocolViolatedException, usedUninitialisedException}
import org.scalatest._

import scala.collection.SortedSet
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{Settings, _}


class PluginTest extends FlatSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll{
  //region <protocol vars init>

  var protocolWalkTwiceIllegal = ""
  var protocolWithNotAMethod = ""
  var protocolWithoutEnd = ""
  var walkLoop3comeAliveLoop1 = ""
  var cannotWalkProtocol = ""
  var walkComeAliveWalkLoopProtocol = ""
  var decisionWalkProtocol = ""
  var pairsProtocol = ""
  var walkComeAliveDifferentProtocol = ""
  var loopProtocol = ""
  var enumProtocol = ""
  //endregion
  /*
    /** Create protocol files before testing */
    override def beforeAll(): Unit = {
      protocolWalkTwiceIllegal =
        """
          |package ProtocolDSL
          |
          |object walkTwiceIllegalProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("walk()") goto "State1"
          |    in ("State1")
          |    end()
          |}
          |""".stripMargin
      writeFile("walkTwiceIllegalProtocol.scala", Seq(protocolWalkTwiceIllegal))
      protocolWithNotAMethod =
        """
          |package ProtocolDSL
          |
          |object withNotAMethodProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("walk()") goto "State1"
          |    in ("State1")
          |    when ("notAMethod()") goto "State1"
          |    end
          |}
          |""".stripMargin
      writeFile("withNotAMethodProtocol.scala", Seq(protocolWithNotAMethod))
      protocolWithoutEnd =
        """
          |package ProtocolDSL
          |
          |object withoutEndProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("walk()") goto "State1"
          |    in ("State1")
          |}
          |""".stripMargin
      writeFile("withoutEndProtocol.scala", Seq(protocolWithoutEnd))
      walkLoop3comeAliveLoop1 =
        """
          |package ProtocolDSL
          |
          |object walkLoop3comeAliveLoop1Protocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("walk()") goto "State1"
          |    when ("comeAlive()") goto "init"
          |    in ("State1")
          |    when("walk()") goto "State2"
          |    in ("State2")
          |    when("walk()") goto "init"
          |    end()
          |}
          |""".stripMargin
      writeFile("walkLoop3comeAliveLoop1Protocol.scala", Seq(walkLoop3comeAliveLoop1))
      cannotWalkProtocol =
        """
          |package ProtocolDSL
          |
          |object cannotWalkProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("comeAlive()") goto "init"
          |    in ("State3")
          |    when("walk()") goto "init"
          |    end()
          |}
          |""".stripMargin
      writeFile("cannotWalkProtocol.scala", Seq(cannotWalkProtocol))
      walkComeAliveWalkLoopProtocol =
        """
          |package ProtocolDSL
          |
          |object walkComeAliveWalkLoopProtocol extends ProtocolLang with App{
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
      writeFile("walkComeAliveWalkLoopProtocol.scala", Seq(walkComeAliveWalkLoopProtocol))
      decisionWalkProtocol =
        """
          |package ProtocolDSL
          |
          |object decisionWalkProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("walk()") goto
          |      "State1" at "true" or
          |      "init" at "false"
          |    in ("State1")
          |    end()
          |}
          |""".stripMargin
      writeFile("decisionWalkProtocol.scala", Seq(decisionWalkProtocol))
      pairsProtocol =
        """
          |package ProtocolDSL
          |
          |object pairsProtocol extends ProtocolLang with App{
          | in("init")
          | when ("setLeft(Int)") goto "leftInitialised"
          |
          | in("leftInitialised")
          | when("setRight(Int)") goto "allInitialised"
          |
          | in("allInitialised")
          | when("sum()") goto "allInitialised"
          |
          | end()
          |}
          |""".stripMargin
      writeFile("pairsProtocol.scala", Seq(pairsProtocol))
      walkComeAliveDifferentProtocol =
        """
          |package ProtocolDSL
          |
          |object walkComeAliveDifferentProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("walk()") goto "State1"
          |    when("comeAlive()") goto "State2"
          |    in ("State1")
          |    in ("State2")
          |    end()
          |}
          |""".stripMargin
      writeFile("walkComeAliveDifferentProtocol.scala", Seq(walkComeAliveDifferentProtocol))
      loopProtocol =
        """
          |package ProtocolDSL
          |
          |object loopProtocol extends ProtocolLang with App {
          |  in("init")
          |  when("finished()") goto "init" at "false" or "end" at "true"
          |  in("end")
          |  end()
          |}
          |""".stripMargin
      writeFile("loopProtocol.scala", Seq(loopProtocol))
      enumProtocol =
        """
          |package ProtocolDSL
          |
          |object enumProtocol extends ProtocolLang with App{
          |    in ("init")
          |    when ("m()") goto
          |        "S2" at "letters.A" or
          |        "S3" at "letters.B" or
          |        "S4" at "letters.C" or
          |        "S5" at "letters.D"
          |    in("S2")
          |    when("go()") goto "S6"
          |    in("S3")
          |    when("grab()") goto "S8"
          |    in("S4")
          |    when("stop()") goto "S7"
          |    in("S5")
          |    when("jump()") goto "S9"
          |    in("S6")
          |    in("S7")
          |    in("S8")
          |    in("S9")
          |    end()
          |}
          |""".stripMargin
      writeFile("enumProtocol.scala", Seq(enumProtocol))
    }
    /** Delete protocol files after testing */
    override def afterAll(): Unit = {
      deleteFile("walkTwiceIllegalProtocol.scala")
      deleteFile("withNotAMethodProtocol.scala")
      deleteFile("withoutEndProtocol.scala")
      deleteFile("walkLoop3comeAliveLoop1Protocol.scala")
      deleteFile("cannotWalkProtocol.scala")
      deleteFile("walkComeAliveWalkLoopProtocol.scala")
      deleteFile("decisionWalkProtocol.scala")
      deleteFile("pairsProtocol.scala")
      deleteFile("walkComeAliveDifferentProtocol.scala")
      deleteFile("loopProtocol.scala")
      deleteFile("enumProtocol.scala")
    }
    */

  //region <Tests>

  //region <BASIC>
  "plugin" should "only recognise the typestate annotation" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |class Nonsense(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Nonsense(filename = "walkTwiceIllegalProtocol")
        |class Trash{
        | def comeAlive():Unit = println("The trash awakens")
        | def walk():Unit = println("walking")
        |}
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  cat.walk()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception when an invalid transition happens in an object" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |object Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  Cat.walk()
        |  Cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("Cat")), "compilerPlugin.Cat.type",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 15, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception when an invalid transition happens in a class in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 15, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception when an invalid transition happens in an object in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |object Cat{
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  Cat.walk()
        |  Cat.walk()
        |}
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("Cat")), "compilerPlugin.Cat.type",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception when protocol methods are not a subset of ones in class" in {
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
        |@Typestate(filename = "withNotAMethodProtocol")
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
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    assert(actualException.getMessage ===
      "Methods Set(walk(), notAMethod()) defined in withNotAMethodProtocol are not a subset of " +
        "methods Set(comeAlive(), walk(), die()) defined in class compilerPlugin.Cat. " +
        "Methods Set(notAMethod()) are defined in the protocol but not in the class")
  }

  "plugin" should "throw an exception if end is not written in the protocol" in {
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
        |@Typestate(filename = "withoutEndProtocol")
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

    assert(actualException.getMessage ===
      "The protocol withoutEndProtocol could not be processed, " +
        "check that the protocol name is the same as the name of the object containing your protocol")
  }

  "plugin" should "deal with multiple classes with multiple protocols" in {
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
        |@Typestate(filename="walkTwiceIllegalProtocol")
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
        |@Typestate(filename = "withNotAMethodProtocol")
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
        |  val dog = new Dog()
        |  dog.walk()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    assert(actualException.getMessage === "Methods Set(walk(), notAMethod()) defined in withNotAMethodProtocol " +
      "are not a subset of methods Set(comeAlive(), walk(), die()) defined in class compilerPlugin.Cat. " +
      "Methods Set(notAMethod()) are defined in the protocol but not in the class")
  }
  //endregion

  //region <FOR LOOPS>
  "plugin" should "throw an exception if an instance violates its protocol inside a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- 1 to 10) cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 16, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance defined inside a for loop violates its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- 1 to 10) {
        |   val kitty = new Cat()
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

    val expectedException = new protocolViolatedException(sortSet(Set("kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance after being in a valid for loop violates its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkLoop3comeAliveLoop1Protocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("State2", 2), State("init",0))), "comeAlive()", "<test>", 21, "walk() ")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "not throw an exception if two instances with looping protocols of different lengths are in a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkLoop3comeAliveLoop1Protocol")
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
        |  cat.comeAlive()
        |  kitty.walk()
        |  }
        |
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region <FOR LOOP GENERATORS>
  "plugin" should "throw an exception if an instance violates its protocol in the to generator of a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the to generator of a for loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the until generator of a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the until generator of a for loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat)) println("for")
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    cat.walk()
        |    List(0,10)
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the multiple generators of a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 30, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the multiple generators  of a for loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 32, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with guard of a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Int = 1
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat) if (i == cat.walk())) println("for")
        |
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
        |    println("inside get cat age range")
        |    cat.walk()
        |    List(0,10)
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 16, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with guard of a for loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with multiple guards of a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Int = 1
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  for(i <- getCatAgeRange(cat) if (i == 0) if(cat.walk() == 0)) println("for")
        |
        |
        |  def getCatAgeRange(cat:Cat): List[Int] ={
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 16, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator with multiple guards of a for loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for yield loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
        |    kitty.walk()
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)

  }

  "plugin" should "throw an exception if an instance violates its protocol in the generator of a for yield loop in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
        |    kitty.walk()
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 23, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  //endregion

  //region <WHILE LOOPS>
  "plugin" should "throw an exception if an instance after being in a valid while loop violates its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkLoop3comeAliveLoop1Protocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("State2", 2), State("init",0))), "comeAlive()", "<test>", 25, "walk() ")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception with the correct states if an instance after being in a valid do-while loop violates its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkLoop3comeAliveLoop1Protocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("init", 0), State("State1", 1), State("State2", 2))), "comeAlive()", "<test>", 22, "walk() ")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if the condition inside the do while causes a protocol violation" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "cannotWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("init", 0))), "walk()", "<test>", 23, "comeAlive() ")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if the condition inside a while causes a protocol violation" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "cannotWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("init", 0))), "walk()", "<test>", 23, "comeAlive() ")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a protocol violation happens after a while(true) loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkLoop3comeAliveLoop1Protocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  val kitty = new Cat()
        |  while(true){
        |  cat.comeAlive()
        |  }
        |  kitty.walk()
        |  kitty.comeAlive()
        |
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region <MIXED LOOPS>
  "plugin" should "throw an exception when inner loop causes a violation" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkComeAliveWalkLoopProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State2", 1))), "comeAlive()", "<test>", 19, "walk() ")
    assert(actualException.getMessage === expectedException.getMessage)
  }
  //endregion

  //region <FUNCTIONS>

  "plugin" should "throw an exception if an instance violates its protocol inside a function" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a nested function" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
        |      makeCatWalkAgain(kitty)
        |      def makeCatWalkAgain(cat:Cat): Unit ={
        |         cat.walk()
        |      }
        |   }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside two nested functions" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  makeCatWalkTwice(cat)
        |
        |  def makeCatWalkTwice(kitty:Cat): Unit ={
        |      kitty.walk()
        |      makeCatWalkAgain(kitty)
        |      def makeCatWalkAgain(cat:Cat): Unit ={
        |         makeCatWalk(cat)
        |         def makeCatWalk(cat:Cat): Unit ={
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a nested function in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
        |      makeCatWalkAgain(kitty)
        |      def makeCatWalkAgain(cat:Cat): Unit ={
        |         cat.walk()
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside two nested functions in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main{
        |def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  makeCatWalkTwice(cat)
        |
        |  def makeCatWalkTwice(kitty:Cat): Unit ={
        |      kitty.walk()
        |      makeCatWalkAgain(kitty)
        |      def makeCatWalkAgain(cat:Cat): Unit ={
        |         makeCatWalk(cat)
        |         def makeCatWalk(cat:Cat): Unit ={
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an outer functions called by an inner one in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
        |    val cat = new Cat()
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 30, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance defined in a function violates its protocol inside a function" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance defined in a function violates its protocol inside a function in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "not throw an exception if two instances with the same name do one legal method each" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
  }

  "plugin" should "throw an exception if a protocolled function changes the state in a different way than the protocol dictates" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkComeAliveDifferentProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = comeAlive()
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    an [inconsistentStateMutation] should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "throw an exception if a protocolled function changes the state in a different way than the protocol dictates in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkComeAliveDifferentProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = comeAlive()
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    an [inconsistentStateMutation] should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region <FUNCTION PARAMETERS>
  "plugin" should "throw an exception if an instance violates its protocol inside a function with one parameter" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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
        |  def takeInt(int:Int): Unit={
        |  }
        |
        |  def makeCatWalk(kitty:Cat): Int ={
        |    kitty.walk()
        |    1
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function with multiple parameters" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function with duplicate parameters" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  val cat2 = new Cat()
        |  makeCatsWalk(cat2, cat2)
        |
        |  def makeCatsWalk(cat:Cat, kat:Cat) ={
        |      cat.walk()
        |      kat.walk()
        |    }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat2","kat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function with parameters returned from functions" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  makeCatsWalk(createCat(), createCat())
        |  def makeCatsWalk(kat:Cat, kitty:Cat): Unit ={
        |    kat.walk()
        |    kat.walk()
        |  }
        |  def createCat(): Cat ={
        |    new Cat()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("kat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a function with parameters returned from functions in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main {
        | def main(args: Array[String]): Unit = {
        |  makeCatsWalk(createCat(), createCat())
        |  def makeCatsWalk(kat:Cat, kitty:Cat): Unit ={
        |    kat.walk()
        |    kat.walk()
        |  }
        |  def createCat(): Cat ={
        |    new Cat()
        |  }
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("kat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  //endregion

  //region <RETURN VALUES>

  "plugin" should "throw an exception if an instance could violate its protocol after a method giving multiple states in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance could violate its protocol after a method giving multiple states" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 16, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  //endregion

  //region <IF ELSE>

  "plugin" should "throw an exception if an instance could violate its protocol after an if else statement in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance could violate its protocol after an if else statement" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an if else statement" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an if else statement in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an if else statement inside a for loop" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol after a singular if statement" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the condition of the if in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the condition of the if" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the  complex condition of the if in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in the complex condition of the if" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 16, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  //endregion

  //region <CODE IN CLASSES AND OBJECTS>
  "plugin" should "throw an exception if an instance violates its protocol inside a class construtor" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("kat", "cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a class construtor in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("kat", "cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol after its constructor" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
        |class Cat{
        |  walk()
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val kat = new Cat()
        |    kat.walk()
        |}
        |
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("kat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 16, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol after its constructor in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
        |class Cat{
        |  walk()
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val kat = new Cat()
        |    kat.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("kat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in its own constructor" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
        |class Cat{
        |  walk()
        |  walk()
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |    val kat = new Cat()
        |}
        |
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("Cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 10, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol in its own constructor in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
        |class Cat{
        |  walk()
        |  walk()
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |    val kat = new Cat()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("Cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 10, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object constructor" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object construtor in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object construtor, alone" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object construtor, alone in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside an object containing main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.Random
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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

    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1), State("init",0))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  //endregion

  //region <TRY CATCH FINALLY>
  "plugin" should "not throw an exception if an instance doesn't violate its protocol inside a try catch" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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
  }

  "plugin" should "not throw an exception if an instance doesn't violate its protocol inside a try catch in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "decisionWalkProtocol")
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
  }
  //endregion

  //region <RETURNING THINGS AND ALIASING>
  "plugin" should "throw an exception if an aliased instance violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat1 = cat
        |  cat1.walk()
        |  val cat2 = cat1
        |  cat2.walk()
        |
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an aliased instance violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat1 = cat
        |  cat1.walk()
        |  val cat2 = cat1
        |  cat2.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance returned from a function violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat1 = createCat
        |  cat1.walk()
        |  cat1.walk()
        |
        |  def createCat(): Cat ={
        |    val kitty = new Cat()
        |    kitty
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance returned from a function violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat1 = createCat
        |  cat1.walk()
        |  cat1.walk()
        |  }
        |
        |  def createCat(): Cat ={
        |    val kitty = new Cat()
        |    kitty
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a used instance returned from a function violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat1 = createCat
        |  cat1.walk()
        |
        |  def createCat(): Cat ={
        |    val kitty = new Cat()
        |    kitty.walk()
        |    kitty
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a used instance returned from a function violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat1 = createCat
        |  cat1.walk()
        |  }
        |
        |  def createCat(): Cat ={
        |    val kitty = new Cat()
        |    kitty.walk()
        |    kitty
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance passed through a function violates its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat1 = makeCatWalk(cat)
        |  cat1.walk()
        |
        |  def makeCatWalk(cat:Cat):Cat ={
        |   cat.walk()
        |   cat
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance passed through a function violates its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat1 = makeCatWalk(cat)
        |  cat1.walk()
        |  }
        |
        |  def makeCatWalk(cat:Cat):Cat ={
        |   cat.walk()
        |   cat
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance aliased via regular assignment violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  var cat1 = new Cat()
        |  var cat2 = new Cat()
        |  cat1 = cat
        |  cat1.walk()
        |  cat2 = cat
        |  cat2.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance aliased via regular assignment violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  var cat = new Cat()
        |  var cat1, cat2 = new Cat()
        |  cat1 = cat
        |  cat1.walk()
        |  cat2 = cat
        |  cat2.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance assigned to itself violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat1 = new Cat()
        |  cat1 = cat1
        |  cat1.walk()
        |  cat1.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance assigned to itself violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  var cat1 = new Cat()
        |  cat1 = cat1
        |  cat1.walk()
        |  cat1.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance assigned to itself in an if/else violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat1 = new Cat()
        |  var x =1
        |  cat1 = if(x == 0)cat1 else cat1
        |  cat1.walk()
        |  cat1.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance assigned to itself in an if/else violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  var cat1 = new Cat()
        |  var cat2 = new Cat()
        |  var x =1
        |  cat1 = if(x == 0) cat1 else cat2
        |  cat2.walk()
        |  cat1.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance assigned to itself from a function violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat1 = new Cat()
        |  cat1 = makeCatWalk(cat1)
        |  cat1.walk()
        |
        |  def makeCatWalk(cat:Cat):Cat ={
        |   cat.walk()
        |   cat
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance assigned to itself from a function violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  var cat1 = new Cat()
        |  cat1 = makeCatWalk(cat1)
        |  cat1.walk()
        |
        |  def makeCatWalk(cat:Cat):Cat ={
        |   cat.walk()
        |   cat
        |  }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance aliased via val = if/else violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  var cat1 = new Cat()
        |  var x = 1
        |  var cat2 = if(x==1) cat else cat1
        |  cat.walk()
        |  cat1.walk()
        |  cat2.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance aliased via val = if/else violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  var cat1 = new Cat()
        |  var x = 1
        |  var cat2 = if(x==1) cat else cat1
        |  cat.walk()
        |  cat1.walk()
        |  cat2.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance aliased via if/else violated its protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat1 = new Cat()
        |  var cat2 = new Cat()
        |  var x = 0
        |  cat2 = if(x==1) cat1 else cat
        |  cat2.walk()
        |  cat1.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance aliased via if/else violated its protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |import java.io.FileNotFoundException
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat1 = new Cat()
        |  var cat2 = new Cat()
        |  var x = 0
        |  cat2 = if(x==1) cat1 else cat
        |  cat2.walk()
        |  cat1.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat1", "cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "not throw an exception in the valid pairs example" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "pairsProtocol")
        |class Pair{
        |  var left:Int=_
        |  var right:Int=_
        |  def setLeft(num:Int): Unit = left = num
        |  def setRight(num:Int): Unit = right = num
        |  def sum(): Int = left + right
        |}
        |
        |object Main extends App{
        |  val p = new Pair()
        |  val p2 = p
        |  p.setLeft(1)
        |  p2.setRight(2)
        |  val result = p.sum()
        |  println(result)
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception in the vaild pairs example in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "pairsProtocol")
        |class Pair{
        |  var left:Int=_
        |  var right:Int=_
        |  def setLeft(num:Int): Unit = left = num
        |  def setRight(num:Int): Unit = right = num
        |  def sum(): Int = left + right
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val p = new Pair()
        |  val p2 = p
        |  p.setLeft(1)
        |  p2.setRight(2)
        |  val result = p.sum()
        |  println(result)
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "throw an exception in the invalid pairs example" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "pairsProtocol")
        |class Pair{
        |  var left:Int=_
        |  var right:Int=_
        |  def setLeft(num:Int): Unit = left = num
        |  def setRight(num:Int): Unit = right = num
        |  def sum(): Int = left + right
        |}
        |
        |object Main extends App{
        |  val p = new Pair()
        |  val p2 = p
        |  p.setLeft(1)
        |  p.sum()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("p", "p2")), "compilerPlugin.Pair",
      sortSet(Set(State("leftInitialised", 1))), "sum()", "<test>", 20, "setRight(Int) ")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  "plugin" should "throw an exception in the invalid pairs example in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "pairsProtocol")
        |class Pair{
        |  var left:Int=_
        |  var right:Int=_
        |  def setLeft(num:Int): Unit = left = num
        |  def setRight(num:Int): Unit = right = num
        |  def sum(): Int = left + right
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val p = new Pair()
        |  val p2 = p
        |  p.setLeft(1)
        |  p.sum()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }

    val expectedException = new protocolViolatedException(sortSet(Set("p", "p2")), "compilerPlugin.Pair",
      sortSet(Set(State("leftInitialised", 1))), "sum()", "<test>", 21, "setRight(Int) ")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  //endregion

  //region <MATCH STATEMENTS>

  "plugin" should "throw an exception if an instance violates its protocol inside a match statement" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  cat.walk() match{
        |   case true =>
        |     cat.walk()
        |   case false =>
        |     cat.walk()
        |   case _ =>
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance violates its protocol inside a match statement in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  cat.walk() match{
        |   case true =>
        |     cat.walk()
        |   case false =>
        |     cat.walk()
        |   case _ =>
        |  }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a match statement matches a return value unspecified by the programmer" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTrueCaseProtocol")
        |class Cat{
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        | val cat = new Cat()
        | cat.walk() match{
        |   case true =>
        |   case false =>
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("init", 1))), "walk():false", "<test>", 14, "walk():true walk():Any ")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "not throw an exception if an instance does not violates its protocol inside a match statement" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  cat match{
        |   case _:Cat =>
        |     cat.walk()
        |   case `cat` =>
        |     cat.walk()
        |   case _ =>
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if an instance does not violates its protocol inside a match statement in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  cat match{
        |   case _:Cat =>
        |     cat.walk()
        |   case `cat` =>
        |     cat.walk()
        |   case _ =>
        |  }
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if an instance does not violates its protocol from a method with boolean return values" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate("loopProtocol")
        |class LoopImpl {
        |  def finished(): Boolean = {true}
        |}
        |
        |object Main{
        | def test2(): Unit = {
        |    test(new LoopImpl)
        |  }
        |
        |  def test(loop: LoopImpl): Unit = {
        |    loop.finished() match {
        |      case false =>
        |        test(loop)
        |      case true =>
        |
        |    }
        |  }
        |  test2()
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if an instance uses match in code by only have single state transition defined for method" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate("walkTwiceIllegalProtocol")
        |class Cat {
        |  def walk():Boolean={true}
        |}
        |
        |object Main extends App{
        |  val cat = new Cat
        |  cat.walk() match{
        |   case true =>
        |   case false =>
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if an instance does not violates its protocol from a method with 4 enum return values" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import compilerPlugin.letters.letter
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |object letters extends Enumeration {
        |  type letter = Value
        |  val A,B,C,D = Value
        |}
        |
        |@Typestate(filename = "enumProtocol")
        |class Cat{
        |  def go() = ???
        |  def grab() = ???
        |  def stop() = ???
        |  def jump() = ???
        |
        |  println("making a cat")
        |  def comeAlive() = println("The cat is alive")
        |  def walk(): Boolean = {
        |    comeAlive()
        |    true
        |  }
        |  def m(): letter ={
        |    letters.A
        |  }
        |}
        |
        |object Main{
        | val cat = new Cat()
        |  cat.m() match{
        |    case letters.A =>
        |      cat.go()
        |    case letters.B =>
        |      cat.grab()
        |    case letters.C =>
        |      cat.stop()
        |    case letters.D =>
        |      cat.jump()
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  //endregion

  //region <RECURSION>
  "plugin" should "not throw an exception if a recursive method taking protocolled values is present" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat1 = new Cat()
        |  val cat2 = new Cat()
        |  makeCatWalk(cat, cat1, cat2)
        |  cat1.walk()
        |  def makeCatWalk(cat:Cat, kitty:Cat, kat:Cat):Cat ={
        |    makeCatWalk(cat, kitty, kat)
        |    cat
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if a recursive method taking protocolled values is present in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat1 = new Cat()
        |  val cat2 = new Cat()
        |  makeCatWalk(cat, cat1, cat2)
        |  cat1.walk()
        |  def makeCatWalk(cat:Cat, kitty:Cat, kat:Cat):Cat ={
        |    makeCatWalk(cat, kitty, kat)
        |    cat
        |  }
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if a recursive method taking duplicate protocolled values is present" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat2 = new Cat()
        |  makeCatWalk(cat, cat2, cat2)
        |  def makeCatWalk(cat:Cat, kitty:Cat, kat:Cat):Cat ={
        |    makeCatWalk(cat, kitty, kat)
        |    cat
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if a recursive method taking duplicate protocolled values is present in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat2 = new Cat()
        |  makeCatWalk(cat, cat2, cat2)
        |  def makeCatWalk(cat:Cat, kitty:Cat, kat:Cat):Cat ={
        |    makeCatWalk(cat, kitty, kat)
        |    cat
        |  }
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if a recursive method is present" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  makeCatWalk()
        |  def makeCatWalk():String ={
        |    makeCatWalk()
        |
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if a recursive method is present in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  makeCatWalk()
        |  def makeCatWalk():String ={
        |    makeCatWalk()
        |  }
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if two mutually recursive methods are present" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  recursive()
        |  def recursive(): Unit ={
        |    recursive2()
        |  }
        |  def recursive2(): Unit ={
        |    recursive()
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if two mutually recursive methods are present in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  recursive()
        |  def recursive(): Unit ={
        |    recursive2()
        |  }
        |  def recursive2(): Unit ={
        |    recursive()
        |  }
        | }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if two mutually recursive methods with parameters are present" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  recursive(cat)
        |  def recursive(cat:Cat): Unit ={
        |    recursive2(cat)
        |  }
        |  def recursive2(cat:Cat): Unit ={
        |    recursive(cat)
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if two mutually recursive methods with parameters are present in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  recursive(cat)
        |  def recursive(cat:Cat): Unit ={
        |    recursive2(cat)
        |  }
        |  def recursive2(cat:Cat): Unit ={
        |    recursive(cat)
        |  }
        | }
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "throw an exception if two mutually recursive methods violate protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  recursive()
        |  def recursive(): Unit ={
        |    recursive2()
        |  }
        |  def recursive2(): Unit ={
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |    recursive()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if two mutually recursive methods violate protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  recursive()
        |  def recursive(): Unit ={
        |    recursive2()
        |  }
        |  def recursive2(): Unit ={
        |    val cat = new Cat()
        |    cat.walk()
        |    cat.walk()
        |    recursive()
        |  }
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if two mutually recursive methods with parameters violate protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  recursive(cat)
        |  def recursive(cat:Cat): Unit ={
        |    cat.walk()
        |    recursive2(cat)
        |  }
        |  def recursive2(cat:Cat): Unit ={
        |    cat.walk()
        |    recursive(cat)
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if two mutually recursive methods with parameters violate protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  recursive(cat)
        |  def recursive(cat:Cat): Unit ={
        |    cat.walk()
        |    recursive2(cat)
        |  }
        |  def recursive2(cat:Cat): Unit ={
        |    cat.walk()
        |    recursive(cat)
        |  }
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 22, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a recursive method taking duplicate protocolled values violates protocol" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  val cat2 = new Cat()
        |  makeCatWalk(cat, cat2, cat2)
        |  def makeCatWalk(cat:Cat, kitty:Cat, kat:Cat):Cat ={
        |    kitty.walk()
        |    makeCatWalk(cat, kitty, kat)
        |    cat
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat2", "kat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a recursive method taking duplicate protocolled values violates protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  val cat2 = new Cat()
        |  makeCatWalk(cat, cat2, cat2)
        |  def makeCatWalk(cat:Cat, kitty:Cat, kat:Cat):Cat ={
        |    kitty.walk()
        |    makeCatWalk(cat, kitty, kat)
        |    cat
        |  }
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat2", "kat", "kitty")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 19, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  //endregion

  //region<COMPANION OBJECTS>
  "plugin" should "throw an exception if a method in a companion object violates protocol " in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Cat{
        |
        |  def makeCatWalk(cat:Cat): Unit ={
        |    cat.walk()
        |    cat.walk()
        |  }
        |
        |}
        |
        |object Main extends App{
        |  val cat = new Cat()
        |  Cat.makeCatWalk(cat)
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if a method in a companion object violates protocol in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Cat{
        |
        |  def makeCatWalk(cat:Cat): Unit ={
        |    println("doing sthg")
        |    cat.walk()
        |    cat.walk()
        |  }
        |
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = new Cat()
        |  Cat.makeCatWalk(cat)
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 18, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if after an apply method in a companion object protocol is violated" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Cat{
        | def apply(): Cat = {
        |    println("making a cat in object")
        |    new Cat()
        |  }
        |
        |}
        |
        |object Main extends App{
        |  val cat = Cat()
        |  cat.walk()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if after an apply method in a companion object protocol is violated in main" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Cat{
        |
        |  def apply(): Cat = {
        |    new Cat()
        |  }
        |
        |}
        |
        |object Main{
        | def main(args: Array[String]): Unit = {
        |  val cat = Cat()
        |  cat.walk()
        |  cat.walk()
        | }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 25, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  //endregion

  //region <break statements>
  "plugin" should "not throw an exception if an instance calls an illegal method after a break statement" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  breakable{
        |   cat.walk()
        |   break()
        |   cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if an instance calls an illegal method after an if statement containing a break" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  breakable{
        |    var x = 0
        |    if(x == 0) {
        |      cat.walk()
        |      break()
        |    }
        |    else{
        |    }
        |    cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "not throw an exception if an instance calls an illegal method after a break of an outer breakable" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Unit = println("walking")
        |}
        |
        |object Main extends App {
        |  val cat = new Cat()
        |  val Outer = Breaks
        |  Outer.breakable{
        |    breakable{
        |      Outer.break()
        |    }
        |    cat.m()
        |    cat.m()
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  "plugin" should "throw an exception if an instance calls an illegal method after an if statement containing a break" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        | val cat = new Cat()
        |  breakable{
        |    var x = 0
        |    if(x == 0) {
        |      cat.walk()
        |      break()
        |    }
        |    else{
        |      cat.walk()
        |    }
        |    cat.walk()
        |  }
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 25, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance calls an illegal method after a nested breakable/break block" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        | val cat = new Cat()
        |  breakable{
        |    breakable{
        |      break()
        |    }
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
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 21, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance calls an illegal method after a labelled nested breakable/break block" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        | val cat = new Cat()
        |  val Outer = Breaks
        |  val Inner = Breaks
        |  Outer.breakable{
        |    Inner.breakable{
        |      Inner.break()
        |    }
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
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  //endregion

  //region <null and _ initialisations>
  "plugin" should "throw an exception if an instance calls a method while it is uninitialised as null" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  def getNull():Cat = null
        |  var cat:Cat = getNull()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[usedUninitialisedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new usedUninitialisedException("walk()", sortSet(Set("cat")), "compilerPlugin.Cat", 18)
    assert(actualException.getMessage == expectedException.getMessage)
  }
  "plugin" should "throw an exception if an instance calls a method while it is uninitialised as _" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat:Cat = _
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[usedUninitialisedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new usedUninitialisedException("walk()", sortSet(Set("cat")), "compilerPlugin.Cat", 17)
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "throw an exception if an instance calls an illegal method after being initialised after being null" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  def getNull():Cat = null
        |  var cat:Cat = getNull()
        |  cat = new Cat
        |  cat.walk()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 20, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }
  "plugin" should "throw an exception if an instance calls an illegal method after being initialised after being _" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat:Cat = _
        |  cat = new Cat
        |  cat.walk()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 17, "No methods are available in this state.")
    assert(actualException.getMessage == expectedException.getMessage)
  }

  "plugin" should "not throw an exception if an instance gets initialised properly after being set to null" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat:Cat = null
        |  cat = new Cat()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "plugin" should "not throw an exception if an instance gets initialised properly after being set to _" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def comeAlive(): Unit = println("The cat is alive")
        |  def walk(): Boolean = true
        |}
        |
        |object Main extends App{
        |  var cat:Cat = _
        |  cat = new Cat()
        |  cat.walk()
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }

  //endregion

  //region <CLASS ATTRIBUTES>
  "plugin" should "throw an exception if an instance calls an illegal method after being a field" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  var friend:Cat = null
        |  def walk(): Boolean = true
        |  def walkFriend(): Unit ={
        |    friend.walk()
        |  }
        |  def setFriend(f:Cat): Unit ={
        |    friend = f
        |  }
        |}
        |
        |object Main extends App{
        |  val cat1 = new Cat()
        |  val cat2 = new Cat()
        |  cat1.setFriend(cat2)
        |  cat1.walkFriend()
        |  cat2.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage === expectedException.getMessage)
  }
  "plugin" should "throw an exception if an instance calls an illegal method after being a field, set with dot" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  var friend:Cat = null
        |  def walk(): Boolean = true
        |  def walkFriend(): Unit ={
        |    friend.walk()
        |  }
        |  def setFriend(f:Cat): Unit ={
        |    friend = f
        |  }
        |}
        |
        |object Main extends App{
        |  val cat1 = new Cat()
        |  val cat2 = new Cat()
        |  cat1.friend = cat2
        |  cat1.walkFriend()
        |  cat2.walk()
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat2")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage === expectedException.getMessage)
  }
  "plugin" should "not throw an exception if an instance with the same name as a field walks once" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  var friend:Cat = null
        |  def walk(): Boolean = true
        |  def walkWithFalseFriend(): Unit ={
        |    val friend = new Cat()
        |    friend.walk()
        |  }
        |  def setFriend(f:Cat): Unit ={
        |    friend = f
        |  }
        |}
        |
        |object Main extends App{
        |  val cat1 = new Cat()
        |  val cat2 = new Cat()
        |  cat1.setFriend(cat2)
        |  cat1.walkWithFalseFriend()
        |  cat2.walk()
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "plugin" should "throw an exception if an instance calls an illegal method as a field inside a non protocolled object" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate(filename = "walkTwiceIllegalProtocol")
        |class Cat{
        |  def walk():Boolean = true
        |}
        |class CatTruck{
        |  var cat:Cat = _
        |  def walkCat() = cat.walk()
        |}
        |
        |object Main extends App{
        |  val catTruck = new CatTruck()
        |  catTruck.cat = new Cat()
        |  catTruck.walkCat
        |  catTruck.walkCat
        |}
        |
        |""".stripMargin
    val actualException = intercept[protocolViolatedException] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    val expectedException = new protocolViolatedException(sortSet(Set("cat")), "compilerPlugin.Cat",
      sortSet(Set(State("State1", 1))), "walk()", "<test>", 24, "No methods are available in this state.")
    assert(actualException.getMessage === expectedException.getMessage)
  }

  //endregion

  //endregion

  //region <Utility functions>

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
  //endregion
}