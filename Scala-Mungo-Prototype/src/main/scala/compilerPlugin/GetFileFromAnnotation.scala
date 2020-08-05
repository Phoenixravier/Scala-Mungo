package compilerPlugin

import java.io.{FileInputStream, IOException, ObjectInputStream}

import scala.io.Source._
import scala.sys.process._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import ProtocolDSL.{Method, ReturnValue, State}

class GetFileFromAnnotation(val global: Global) extends Plugin {
  import global._

  val name = "GetFileFromAnnotation"
  val description = "gets file from typestate annotation"
  val components: List[PluginComponent] = List[PluginComponent](Component)
  var data: (Array[Array[State]], Array[State], Array[ReturnValue]) = _

  private object Component extends PluginComponent {
    val global: GetFileFromAnnotation.this.global.type = GetFileFromAnnotation.this.global
    val runsAfter: List[String] = List[String]("parser")
    val phaseName: String = GetFileFromAnnotation.this.name
    def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

    class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
      override def name: String = GetFileFromAnnotation.this.name

      def apply(unit: CompilationUnit): Unit = {
        for (tree@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" <- unit.body) {
          val annotations = mods.annotations
          for(annotation@Apply(arg1,arg2) <- annotations){
            getFilenameFromAnnotation(annotation) match{
              case Some(filename) => {
                //print the contents of the file out to the console
                printFile(filename)
                //execute the DSL in the protocol file and serialize the data into a file
                executeFile(filename)
                //retrieve the serialized data
                data = getDataFromFile("protocolDir\\EncodedData.ser")
                println("Decoded array ", data)
              }
              case None => println("Not a compilerPlugin.Typestate annotation")
            }
          }
        }
        data match{
          case null => return
          case _ =>
        }
        val (transitionsArray, statesArray, returnValuesArray) = data
        println(transitionsArray)
        println(statesArray)
        println(returnValuesArray)
        println("Keep going with the program here")
      }

      def getFilenameFromAnnotation(annotation: Apply): Option[String] ={
        annotation match{
          case Apply(Select(New(Ident(TypeName("Typestate"))), con),List(NamedArg(Ident(TermName("filename")), Literal(Constant(filename))))) => Some(filename.toString)
          case Apply(Select(New(Ident(TypeName("Typestate"))), con),List(Literal(Constant(filename)))) => Some(filename.toString)
          case _ => None
        }
      }

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

      def executeFile(filename:String): Unit ={
        "executeUserProtocol.bat".!
      }

      def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
        val ois = new ObjectInputStream(new FileInputStream(filename))
        val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
        ois.close
        stock
      }

    }
  }
}