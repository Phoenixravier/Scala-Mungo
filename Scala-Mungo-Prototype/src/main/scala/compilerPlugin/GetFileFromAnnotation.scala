package compilerPlugin

import java.io.{FileInputStream, IOException, ObjectInputStream}

import scala.io.Source._
import scala.sys.process._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import ProtocolDSL.{Method, ReturnValue, State}

import scala.::
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees
import scala.util.control.Breaks


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
                val methodToIndices = createMethodToIndicesMap(returnValuesArray)
                println(methodToIndices)
                checkClassIsUsedCorrectly(unit, transitionsArray, statesArray, methodToIndices)
              }
              case None => println("Not a compilerPlugin.Typestate annotation")
            }
          }
        }
      }

      def createMethodToIndicesMap(returnValuesArray:Array[ReturnValue]): mutable.HashMap[String, Set[Int]] ={
        var methodToIndices:mutable.HashMap[String, Set[Int]] = mutable.HashMap()
        for(returnValue <- returnValuesArray){
          var name = ""
          val loop = new Breaks
          loop.breakable {
            for (c <- returnValue.parentMethod.name) {
              if (c != '(') {
                name = name.concat(c.toString)
              }
              else loop.break
            }
          }
          methodToIndices += (name -> returnValue.parentMethod.indices)
        }
        methodToIndices
      }

      def checkClassIsUsedCorrectly(unit:CompilationUnit, transitionsArray: Array[Array[State]], statesArray:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]]): Unit ={
        for(tree@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- unit.body){
          for(parent <- parents){
            if(parent.toString() == "App") {
              checkBody(body, transitionsArray, statesArray, methodToIndices)
            }
          }
          for (definition <- body){
            definition match{
              case q"$mods def main[..$tparams](args: Array[String]): $tpt = $expr" => {
                checkExpr(expr)
              }
              case _ =>
            }
          }
        }
      }

      class InstanceWithState(var name:String, var currentState:State){
        def updateCurrentState(state:State): Unit ={
          this.currentState = state
        }
        override def toString(): String={
          this.name +" "+ this.currentState
        }
      }

      def checkBody(code:Seq[Trees#Tree], transitionsArray:Array[Array[State]], statesArray:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]]): Unit ={
        var instances:Set[InstanceWithState] = Set()
        for(line <- code){
          line match{
            case q"$mods val $tname: $tpt = new Cat(...$exprss)" => instances += new InstanceWithState(tname.toString(), statesArray(0))
            case q"$mods var $tname: $tpt = new Cat(...$exprss)" => instances += new InstanceWithState(tname.toString(), statesArray(0))
            case _ => updateStateIfNeeded(instances, transitionsArray, line, methodToIndices)
          }
        }
        instances.foreach(println)
      }

      def updateStateIfNeeded(values: Set[GetFileFromAnnotationPhase.this.InstanceWithState], transitionsArray: Array[Array[State]], line:Trees#Tree, methodToIndices:mutable.HashMap[String, Set[Int]]): Unit ={
        line match{
          case app@Apply(fun, args) => traverser.traverse(app)
          case _ =>
        }
        val catCalls = traverser.catcalls
        for(catCall <- catCalls){
          catCall match{
            case Select(Ident(TermName("cat")), TermName(methodName)) => {
              var stateIndex:Int = -1
              for(value <- values){
                if(value.name == "cat") stateIndex = value.currentState.index
              }
              println(methodName)
              if(methodToIndices.contains(methodName)) {
                val indiceSet = methodToIndices(methodName)

                println(transitionsArray(stateIndex)(indiceSet.head))
              }
            }
            case _ =>
          }
        }
        //reset the traverser's list to be empty
        traverser.catcalls = ListBuffer[Trees#Tree]()
      }

      object traverser extends Traverser {
        var catcalls = ListBuffer[Trees#Tree]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case app@Apply(fun, args) =>
              app match {
                case q"$expr(...$exprss)" =>
                  expr.children match {
                    case List(Ident(TermName("cat"))) => catcalls += expr
                    case _ =>
                  }
                case _ =>
              }
              super.traverse(fun)
              super.traverseTrees(args)
            case _ =>
              super.traverse(tree)
          }
        }
      }


      def checkExpr(codeBlock:Any): Unit ={}

      object Exe extends Enumeration{
        type Exe = Value
        val None, MainMethod, App = Value
      }


      def ckType(s:String): Unit ={
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
            case _ =>
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