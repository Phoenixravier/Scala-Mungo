package compilerPlugin

import java.io.{FileInputStream, IOException, ObjectInputStream}

import scala.io.Source._
import scala.sys.process._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import ProtocolDSL.{ReturnValue, State}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees
import java.io.File

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
        var setOfClassesWithProtocols: Set[String] = Set()
        println(showRaw(unit.body))
        for (tree@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" <- unit.body) {
          val annotations = mods.annotations
          for(annotation@Apply(arg1,arg2) <- annotations){
            getFilenameFromAnnotation(annotation) match{
              case Some(filename) => { //a correct Typestate annotation is being used
                //execute the DSL in the protocol file and serialize the data into a file
                executeFile(filename) //UNCOMMENT THIS TO GET NEW DATA FROM THE PROTOCOL FILE, COMMENT TO TEST MUCH FASTER
                //retrieve the serialized data
                val className = tpname.toString()
                setOfClassesWithProtocols += className
                println(setOfClassesWithProtocols)
                val (transitionsArray, statesArray, returnValuesArray) = getDataFromFile("protocolDir\\EncodedData.ser")
                checkMethodsAreSubset(returnValuesArray, stats, className, filename)
                val methodToIndices = createMethodToIndicesMap(returnValuesArray)
                println(methodToIndices)
                checkClassIsUsedCorrectly(className, unit, transitionsArray, statesArray, methodToIndices)
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

      def checkClassIsUsedCorrectly(className:String, unit:CompilationUnit, transitionsArray: Array[Array[State]], statesArray:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]]): Unit ={
        for(tree@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- unit.body){
          for(parent <- parents){
            if(parent.toString() == "App") {
              checkBody(className, body, transitionsArray, statesArray, methodToIndices)
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

      class InstanceWithState(var className: String, var name:String, var currentState:State){
        def updateCurrentState(state:State): Unit ={
          this.currentState = state
        }
        override def toString(): String={
          this.className + " " + this.name +" "+ this.currentState
        }
      }

      def checkBody(className:String, code:Seq[Trees#Tree], transitionsArray:Array[Array[State]], statesArray:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]]): Unit ={
        var instances:Set[InstanceWithState] = Set()
        statesArray.foreach(println)
        for(line <- code){
          line match {
            case q"""$mods val $tname: $tpt = new $classNm(...$exprss)""" =>
              if (classNm.toString() == className) {
                instances += new InstanceWithState(className, tname.toString(), statesArray(0))
              }
            case q"$mods var $tname: $tpt = new $classNm(...$exprss)" =>
              if (classNm.toString() == className) {
                instances += new InstanceWithState(className, tname.toString(), statesArray(0))
              }
            case _ => updateStateIfNeeded(className, instances, transitionsArray, line, methodToIndices)
          }
        }
        instances.foreach(println)
      }

      def updateStateIfNeeded(className:String, values: Set[GetFileFromAnnotationPhase.this.InstanceWithState], transitionsArray: Array[Array[State]], line:Trees#Tree, methodToStateIndices:mutable.HashMap[String, Set[Int]]): Unit ={
        line match{
          case app@Apply(fun, args) => exprTraverser.traverse(app)
          case _ =>
        }
        val exprCalls = exprTraverser.exprcalls
        for(exprCall <- exprCalls){
          exprCall match{
            case Select(Ident(TermName(instanceName)), TermName(methodName)) =>
              for(value <- values){
                if(value.name == instanceName) {
                  val stateIndex = value.currentState.index
                  val stateName = value.currentState.name
                  if (methodToStateIndices.contains(methodName)) {
                    val indiceSet = methodToStateIndices(methodName)
                    val state = transitionsArray(stateIndex)(indiceSet.head)
                    if(state == null) throw new Exception(s"Invalid transition in object $instanceName of type $className from state $stateName with method $methodName")
                    value.updateCurrentState(state)
                  }
                }
              }
            case _ =>
          }
        }
        //reset the traverser's list to be empty
        exprTraverser.exprcalls = ListBuffer[Trees#Tree]()
      }

      object constructorTraverser extends Traverser{
        var constructors = ListBuffer[Trees#Tree]()

        override def traverse(tree: Tree): Unit = {

        }
      }

      object exprTraverser extends Traverser {
        var exprcalls = ListBuffer[Trees#Tree]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case app@Apply(fun, args) =>
              app match {
                case q"$expr(...$exprss)" =>
                  expr match {
                    case Select(Ident(TermName(name)), method) => exprcalls += expr
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
        val file = new File(filename)
        file.delete
        stock
      }

    }
  }
}