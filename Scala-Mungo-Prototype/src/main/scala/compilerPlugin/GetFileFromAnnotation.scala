package compilerPlugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.io.Source._
import scala.collection.mutable.ListBuffer
//import scala.meta.tokens.Token.Colon
//import scala.meta._

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
        val source = fromFile(filename).getLines
        while (source.hasNext)
          println(source.next())
      }

      def apply(unit: CompilationUnit): Unit = {


        for (tree@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" <- unit.body) {
          //global.reporter.echo(tree.pos, s"mods.annotations=${mods.annotations}")
          val annotations = mods.annotations
          annotations.foreach({ a => println(a.toString().contains("Typestate")) })

          val typestateAnnotation = annotations.filter(_.toString().contains("Typestate"))

          println(typestateAnnotation)

          if(typestateAnnotation.size > 0){  //there is almost certainly a better way of writing this but it works <3
            for(annotation@Apply(arg1, arg2) <- typestateAnnotation){
              for(file@q"filename = $filename" <- annotation){
                println("filename is "+filename)
                var value = ""
                filename match {
                  case Literal(Constant(value)) => printFile(value.toString)
                  case _ => println("failed")
                }
                //global.reporter.echo(tree.pos, s"$filename")
              }
            }
          }

          //global.reporter.echo(tree.pos, tree.symbol.annotations.mkString(", "))
        }

      }

    }
  }
}