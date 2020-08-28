
import java.io.{BufferedWriter, ByteArrayOutputStream, File, FileWriter}

import compilerPlugin.GetFileFromAnnotation

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{Global, Settings}
import org.scalatest._


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
        |@Nonsense(filename = "TrashProtocol.scala")
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
        |  val cat = new Cat()
        |  cat.walk()
        |  cat.walk()
        |
        |}
        |
        |""".stripMargin
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("FelineProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(State1) with method walk()")
  }

  "plugin" should "throw an exception when an invalid transition happens" in {
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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(State3) with method comeAlive()")
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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "The protocol at \"MyProtocol.scala\" could not be processed, " +
        "check you have an end statement at the end of the protocol")
  }

  /*
  LOOPS
   */

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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(State3) with method walk()")
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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object kitty of type Cat from state(s) Set(State3) with method walk()")
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
        |
        |}
        |
        |""".stripMargin
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(init, State3, State2) with method comeAlive()")
  }

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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(init, State3, State2) with method comeAlive()")
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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(State2, State3) with method comeAlive()")
  }

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
    val thrown = intercept[Exception] {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
    deleteFile("MyProtocol.scala")
    assert(thrown.getMessage ===
      "Invalid transition in object cat of type Cat from state(s) Set(State2) with method comeAlive()")
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

}

