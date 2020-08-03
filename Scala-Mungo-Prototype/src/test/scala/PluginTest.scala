
import java.io.ByteArrayOutputStream

import compilerPlugin.{GetFileFromAnnotation, ProtocolLang}

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{Global, Settings}
import org.scalatest._


import scala.sys.process._

class PluginTest extends FlatSpec with Matchers {
    "at init object" should "have correct values" in {
      makeProtocolFile()

      val code = """
               |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
               |
               |@Typestate(filename = "MyProtocol.txt")
               |class Cat{
               |  def comeAlive(): Unit = println("The cat is alive")
               |}
               |
               |object Main extends App {
               |  val cat = new Cat()
               |  cat.comeAlive()
               |}""".stripMargin
      val (compiler, sources) = createCompiler(code)
      new compiler.Run() compileSources (sources)
      val out = new ByteArrayOutputStream
      Console.withOut(out)(new compiler.Run() compileSources (sources))
      assert(out.toString.trim == "This is a test")
  }


  def createCompiler(code:String): (Global, List[BatchSourceFile]) ={
    val sources = List(new BatchSourceFile("<test>", code))
    println("sources "+sources)

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

  def makeProtocolFile(): Unit ={
    "src\\test\\scala\\makeProtocolFile.bat".!
  }

}

