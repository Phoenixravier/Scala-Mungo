
import java.io.{BufferedWriter, File, FileWriter}

import ProtocolDSL.State
import compilerPlugin.{GetFileFromAnnotation, inconsistentStateMutation, protocolViolatedException}
import org.scalatest._

import scala.collection.SortedSet
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Settings, _}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter


class ExamplesTest extends FlatSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll{

  //region examples
  "adder" should "work" in {
    val userCode =
      """
        |
        |""".stripMargin
  }
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





