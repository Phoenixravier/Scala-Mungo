package compilerPlugin

import java.io.{FileInputStream, IOException, ObjectInputStream}

import scala.io.Source._
import scala.sys.process._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import ProtocolDSL.{Method, ReturnValue, State}

import scala.::
import scala.collection.mutable.ArrayBuffer
import scala.reflect.api.Trees


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
              case Some(filename) => { //a correct Typestate annotation is being used
                //execute the DSL in the protocol file and serialize the data into a file
                //executeFile(filename) //UNCOMMENT THIS TO GET NEW DATA FROM THE PROTOCOL FILE, COMMENT TO TEST MUCH FASTER
                //retrieve the serialized data
                val (transitionsArray, statesArray, returnValuesArray) = getDataFromFile("protocolDir\\EncodedData.ser")
                checkMethodsAreSubset(returnValuesArray, stats, tpname.toString(), filename)
                checkClassIsUsedCorrectly(unit, transitionsArray, statesArray, returnValuesArray)
              }
              case None => println("Not a compilerPlugin.Typestate annotation")
            }
          }
        }
      }

      def checkClassIsUsedCorrectly(unit:CompilationUnit, transitionsArray: Array[Array[State]], statesArray:Array[State], returnValuesArray:Array[ReturnValue]): Unit ={
        for(tree@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- unit.body){
          for(parent <- parents){
            if(parent.toString() == "App") println(body)
          }
          for (definition <- body){
            definition match{
              case q"$mods def main[..$tparams](args: Array[String]): $tpt = $expr" => println(showRaw(expr))
              case _ =>
            }
          }
        }
      }

      def method(s:String): Unit ={
        println("hi")
      }

      def checkMethodsAreSubset(returnValuesArray:Array[ReturnValue], stats: Seq[Trees#Tree], className:String, filename:String): Unit ={
        val classMethodSignatures = getMethodNames(stats)
        println(classMethodSignatures)
        var protocolMethodSignatures: Set[String] = Set()
        for(i <- returnValuesArray.indices){
          protocolMethodSignatures += returnValuesArray(i).parentMethod.name.replaceAll("\\s", "")
        }
        println(protocolMethodSignatures)
        if(!(protocolMethodSignatures subsetOf classMethodSignatures)) throw new Exception(
          s"Methods $protocolMethodSignatures defined in $filename are not a subset of methods $classMethodSignatures defined in class $className")
      }

      def getFilenameFromAnnotation(annotation: Apply): Option[String] ={
        annotation match{
          case Apply(Select(New(Ident(TypeName("Typestate"))), con),List(NamedArg(Ident(TermName("filename")), Literal(Constant(filename))))) => Some(filename.toString)
          case Apply(Select(New(Ident(TypeName("Typestate"))), con),List(Literal(Constant(filename)))) => Some(filename.toString)
          case _ => None
        }
      }

      def getMethodNames(stats: Seq[Trees#Tree]): Set[String]={
        var methodNames: Set[String] = Set()
        for(method <- stats){
          method match{
            case DefDef(mod, TermName(methodName), tparams, vparams, Ident(TypeName(returnType)), commands) => {
              val parameters = getParameters(vparams)
              methodNames += methodName+s"($parameters):"+returnType
            }
            case _ => println("not matched")
          }
        }
        methodNames
      }

      def getParameters(params:List[List[ValDef]]): String ={
        params match{
          case List(List()) => ""
          case List(List(value)) => value.tpt.toString()
          case List(values) => {
            var parameters:ArrayBuffer[String] = ArrayBuffer()
            for(elem <- values){
              parameters += elem.tpt.toString
            }
            parameters.mkString(",")
          }
          case _ => ""
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
        s"executeUserProtocol.bat $filename".!
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