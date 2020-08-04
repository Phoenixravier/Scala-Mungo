
import java.io.{BufferedWriter, ByteArrayOutputStream, File, FileWriter}

import compilerPlugin.GetFileFromAnnotation

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{Global, Settings}
import org.scalatest._

class PluginTest extends FlatSpec with Matchers {
    "at init object" should "have correct values" in {
      val protocolText =
        """package ProtocolDSL
          |
          |import compilerPlugin.ProtocolLang
          |
          |
          |object Example extends ProtocolLang{
          |  def main(args:Array[String]) = {
          |    in ("State0")
          |    when ("walk()") goto "State3"
          |    when ("comeAlive()") goto "State0"
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
          |}""".stripMargin

      writeFile("MyProtocol.scala", Seq(protocolText))

      val userCode = """
               |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
               |
               |@Typestate(filename = "MyProtocol.scala")
               |class Cat{
               |  def comeAlive(): Unit = println("The cat is alive")
               |}
               |
               |object Main extends App {
               |  val cat = new Cat()
               |  cat.comeAlive()
               |}""".stripMargin

      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
      val out = new ByteArrayOutputStream
      Console.withOut(out)(new compiler.Run() compileSources (sources))

      assert(out.toString.trim == protocolText)
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
}

