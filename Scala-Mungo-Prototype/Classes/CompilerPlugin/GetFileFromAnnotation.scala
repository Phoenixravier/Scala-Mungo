package CompilerPlugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class GetFileFromAnnotation(val global: Global) extends Plugin {
  import global._

  val name = "divbyzero"
  val description = "checks for division by zero"
  val components: List[PluginComponent] = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: GetFileFromAnnotation.this.global.type = GetFileFromAnnotation.this.global
    val runsAfter: List[String] = List[String]("refchecks")
    val phaseName: String = GetFileFromAnnotation.this.name
    def newPhase(_prev: Phase) = new DivByZeroPhase(_prev)

    class DivByZeroPhase(prev: Phase) extends StdPhase(prev) {
      override def name: String = GetFileFromAnnotation.this.name

      def apply(unit: CompilationUnit): Unit = {
        for ( tree @ Apply(Select(New(Ident("Typestate")), rcvr), List(Literal(Constant(filename)))) <- unit.body)
        {
          global.reporter.error(tree.pos, "file name is here")
        }
      }

    }
  }
}