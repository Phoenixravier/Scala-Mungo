package compilerPlugin

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.{Global, Settings}

object AnnotationFinderTest extends App {
  // prepare the code you want to compile
  val code = "object Foo extends Application { println(42 / 0) }"
  val sources = List(new BatchSourceFile("<test>", code))
  println("sources "+sources)
  val settings = new Settings
  settings.usejavacp.value = true

  /**
  val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
  val entries = loader.getURLs map(_.getPath)
  // annoyingly, the Scala library is not in our classpath, so we have to add it manually
  val sclpath = entries find(_.endsWith("scala-compiler.jar")) map(
    _.replaceAll("scala-compiler.jar", "scala-library.jar"))
  settings.classpath.value = ClassPath.join((entries ++ sclpath) : _*)
  // save class files to a virtual directory in memory
   **/

  settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))

  /**
  val compiler = new Global(settings, new ConsoleReporter(settings)) {
    override protected def computeInternalPhases () {
      super.computeInternalPhases
      for (phase <- new GetFileFromAnnotation(this).components)
        phasesSet += phase
    }
  }
   **/

  val compiler = new Global(settings, new ConsoleReporter(settings)) {
    override protected def loadRoughPluginsList: List[Plugin] =
      new GetFileFromAnnotation(this) :: super.loadRoughPluginsList
  }


    new compiler.Run() compileSources (sources)

}
