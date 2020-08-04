package compilerPlugin

import java.io.IOException
import scala.io.Source._
import scala.sys.process._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}


class GetFileFromAnnotation(val global: Global) extends Plugin {
  import global._

  val name = "GetFileFromAnnotation"
  val description = "gets file from typestate annotation"
  val components: List[PluginComponent] = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: GetFileFromAnnotation.this.global.type = GetFileFromAnnotation.this.global
    val runsAfter: List[String] = List[String]("parser")
    val phaseName: String = GetFileFromAnnotation.this.name
    def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

    class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
      override def name: String = GetFileFromAnnotation.this.name

      def printFile(filename: String): Unit ={
        val source = fromFile(filename)
        try {
          val it = source.getLines()
          while (it.hasNext)
            println(it.next())
        }
        catch{
          case e: IOException => println(s"Had an IOException trying to use file $filename")
        } finally {
          source.close
        }
      }

      def getArrayFromFile(filename: String): Unit ={
        val source = fromFile(filename)
        try {
          val it = source.getLines()
          while (it.hasNext)
            println(it.next())
        }
        catch{
          case e: IOException => println(s"Had an IOException trying to use file $filename")
        } finally {
          source.close
        }
      }

      def getFilenameFromAnnotation(annotation: Apply): Option[String] ={
        annotation match{
          case Apply(Select(New(Ident(TypeName("Typestate"))), con),List(NamedArg(Ident(TermName("filename")), Literal(Constant(filename))))) => Some(filename.toString)
          case Apply(Select(New(Ident(TypeName("Typestate"))), con),List(Literal(Constant(filename)))) => Some(filename.toString)
          case _ => None
        }
      }

      def apply(unit: CompilationUnit): Unit = {
        for (tree@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" <- unit.body) {
          val annotations = mods.annotations
          for(annotation@Apply(arg1,arg2) <- annotations){
            getFilenameFromAnnotation(annotation) match{
              case Some(filename) => {
                printFile(filename)
              }
              case None => println("Not a compilerPlugin.Typestate annotation")
            }
          }
        }
      }
    }
  }
}