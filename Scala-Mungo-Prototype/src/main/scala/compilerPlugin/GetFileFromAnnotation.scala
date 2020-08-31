package compilerPlugin

import java.io.{File, FileInputStream, ObjectInputStream}
import java.nio.file.{Files, Paths}

import scala.sys.process._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import ProtocolDSL.{ReturnValue, State}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees
import util.control.Breaks._

case class Instance(var className: String, var name:String, var currentStates:Set[State], var scope: String){
  def updateState(stateToRemove:State, stateToAdd:State): Unit ={
    this.currentStates -= stateToRemove
    this.currentStates += stateToAdd
  }
  override def toString(): String={
    this.className + " " + this.name +" "+ this.currentStates+" "+scope
  }
}

case class ClassInfo(className:String, transitions:Array[Array[State]], states:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]], isObject:Boolean=false){
  override def toString(): String={
    this.className + " " + transitions.foreach(_.mkString(", ")) + " " + states.mkString(", ") + " " + methodToIndices + " " + isObject
  }
}


class GetFileFromAnnotation(val global: Global) extends Plugin {
  val name = "GetFileFromAnnotation"
  val description = "Checks the protocol defined on a class or object with the Typestate annotation"
  lazy val components =
    new MyComponent(this, global) :: Nil
}

class MyComponent(plugin: GetFileFromAnnotation, val global: Global) extends PluginComponent {
    import global._
    case class Function(name:String, body:Tree, scope:String){
      override def toString():String={
        this.name + " " + this.scope
      }
    }
    val runsAfter: List[String] = List[String]("refchecks")
    val phaseName: String = "compilerPlugin.GetFileFromAnnotation.this.name"
    def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

    class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
      var compilationUnit:CompilationUnit =_
      val Undefined = "_Undefined_"
      override def name: String = "compilerPlugin.GetFileFromAnnotation.this.name"

      def getType[T: TypeTag](obj: T) = typeOf[T]

      def apply(unit: CompilationUnit): Unit = {
        functionTraverser.traverse(unit.body)
        println(s"functions are ${functionTraverser.functions}")
        this.compilationUnit = unit
        var setOfClassesWithProtocols: Set[String] = Set()
        for (tree@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" <- unit.body) {
          checkElement(stats, tpname.toString(), tree) match{
            case Some(className) => setOfClassesWithProtocols += className
            case None =>
          }
        }
        for(tree@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- unit.body){
          checkElement(body, tname.toString(), tree, true) match{
            case Some(objectName) => setOfClassesWithProtocols += objectName
            case None =>
          }
        }
      }

      def checkElement(body:Seq[Trees#Tree], name:String, tree:Tree, isObject:Boolean=false): Option[String] ={
        val annotations = tree.symbol.annotations
        for(annotation@AnnotationInfo(arg1,arg2, arg3) <- annotations){
          getFilenameFromTypestateAnnotation(annotation) match{
            case Some(filename) => { //a correct Typestate annotation is being used
              //execute the DSL in the protocol file and serialize the data into a file
              executeFile(filename)
              //retrieve the serialized data
              if(!Files.exists(Paths.get("protocolDir\\EncodedData.ser")))
                throw new Exception(s"The protocol at $filename could not be processed, " +
                  s"check you have an end statement at the end of the protocol")
              val (transitions, states, returnValuesArray) = getDataFromFile("protocolDir\\EncodedData.ser")
              cleanProject()
              checkMethodsAreSubset(returnValuesArray, body, name, filename)
              val methodToIndices = createMethodToIndicesMap(returnValuesArray)
              val classInfo = ClassInfo(name, transitions, states, methodToIndices, isObject)
              println("checking class "+classInfo.className)
              checkClassIsUsedCorrectly(classInfo)
              Some(name)
            }
            case None => None
          }
        }
        None
      }

      /** Creates a hashmap from method names (e.g. "walk(String)")
       * to indices at which it is present in the transitions array (including all the return values it might have)
       *
       * @param returnValuesArray:Array[ReturnValue]
       * @return mutable.Hashmap[String, Set[Int]]
       */
      def createMethodToIndicesMap(returnValuesArray:Array[ReturnValue]): mutable.HashMap[String, Set[Int]] ={
        var methodToIndices:mutable.HashMap[String, Set[Int]] = mutable.HashMap()
        returnValuesArray.foreach((value: ReturnValue) => print(value+ " "))
        println("")
        for(returnValue <- returnValuesArray){
          methodToIndices += (stripReturnValue(returnValue.parentMethod.name) -> returnValue.parentMethod.indices)
        }
        methodToIndices
      }

      /** Checks that a class is following its protocol
       * Severely limited at the moment
       *
       * Does not deal with loops
       * Does not deal with function calls
       * Does not deal with return values (and subsequent path possibilities)
       * Does not work with methods with parameters
       * Only works with code inside "App"
       * */
      def checkClassIsUsedCorrectly(classInfo:ClassInfo): Unit ={
        for(tree@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body){
          for(parent <- parents){
            if(parent.toString() == "App") {
              checkBody(classInfo, body)
            }
          }
          for (definition <- body){
            definition match{
              case q"$mods def main[..$tparams](...$paramss): $tpt = $expr" =>
                if(getParameters(paramss) == "Array[String]") checkExpr(classInfo, expr)
              case _ =>
            }
          }
        }
      }

      /** Goes inside "App" object to see if there are instances with protocols and if they are following their protocol
       *
       * @param classInfo
       * @param code
       */
      def checkBody(classInfo:ClassInfo, code:Seq[Trees#Tree], givenInstances:Set[Instance]=Set()): Unit = {
        var instances: Set[Instance] = givenInstances
        if(classInfo.isObject) instances +=
          Instance(classInfo.className, classInfo.className, Set(classInfo.states(0)), "")
        for (line <- code) {
            val newInstanceAndNbLinesToSkip = processLine(line, instances, classInfo)
            newInstanceAndNbLinesToSkip._1 match {
              case Some(instance) => instances += instance
              case None =>
            }
        }
        println("\nInstances:")
        instances.foreach(println)
      }

      def checkExpr(classInfo:ClassInfo, code:Trees#Tree, givenInstances:Set[Instance]=Set()): Unit ={
        var instances: Set[Instance] = givenInstances
        if(classInfo.isObject) instances +=
          Instance(classInfo.className, classInfo.className, Set(classInfo.states(0)), "")
        var nbOfLinesToSkip = 0
        for (line <- code) {
          breakable {
            if(nbOfLinesToSkip>0){
              nbOfLinesToSkip-=1
              break
            }
            val newInstanceAndNbLinesToSkip = processLine(line, instances, classInfo)
            newInstanceAndNbLinesToSkip._1 match {
              case Some(instance) => instances += instance
              case None =>
            }
            nbOfLinesToSkip = newInstanceAndNbLinesToSkip._2
          }
        }
        println("\nInstances:")
        instances.foreach(println)
      }

      def processLine(line:Trees#Tree, instances: Set[Instance], classInfo:ClassInfo): (Option[Instance], Int) ={
        val className = classInfo.className
        val states = classInfo.states
        line match {
          case q"$mods val $tname: $tpt = new $classNm(...$exprss)" =>
            if (classNm.symbol.name.toString == className) {
              (Some(Instance(className, tname.toString(), Set(states(0)), "")),0)
            } else (None,0)
          case q"$mods var $tname: $tpt = new $classNm(...$exprss)" =>
            if (classNm.symbol.name.toString == className) {
              (Some(Instance(className, tname.toString(), Set(states(0)), "")),0)
            } else (None,0)
          case q"for (..$enums) $expr" => {
            dealWithLoopContents(classInfo, instances, expr)
            var nbOfLinesToSkip = 0
            for(lineLine <- line) nbOfLinesToSkip+=1
            nbOfLinesToSkip-=1 //because we are processing the current one already
            (None, nbOfLinesToSkip)
          }
          case LabelDef(
          TermName(name),
          List(),
          block @ Block(statements, Apply(Ident(TermName(name2)), List())))
            if (name.startsWith("while$") || name.startsWith("doWhile$")) && name2 == name =>{
            println("IN TRUE while loop")
            dealWithLoopContents(classInfo, instances, block.asInstanceOf[Trees#Tree])
            var nbOfLinesToSkip = 0
            for(lineLine <- line) nbOfLinesToSkip+=1
            nbOfLinesToSkip-=1 //because we are processing the current one already
            (None, nbOfLinesToSkip)
          }
          case q"while ($cond) $expr" =>{
            dealWithLoopContents(classInfo, instances, expr)
            var nbOfLinesToSkip = 0
            for(lineLine <- line) nbOfLinesToSkip+=1
            nbOfLinesToSkip-=1 //because we are processing the current one already
            (None, nbOfLinesToSkip)
          }
          case q"do $expr while ($cond)" =>{
            dealWithLoopContents(classInfo, instances, expr)
            checkExpr(classInfo, expr, instances)
            (None, 0)
          }
          case q"for (..$enums) yield $expr" =>{
            dealWithLoopContents(classInfo, instances, expr)
            var nbOfLinesToSkip = 0
            for(lineLine <- line) nbOfLinesToSkip+=1
            nbOfLinesToSkip-=1 //because we are processing the current one already
            (None, nbOfLinesToSkip)
          }
          case app@Apply(Select(calledOn, functionName), args) =>{
            printBanner()
            val functionScope = app.symbol.fullNameString
            var instanceScope = ""
            if(calledOn.hasSymbolField) {
              println("owner is "+calledOn.symbol.owner.ownerChain)
              //instanceScope = calledOn.symbol.owner
            }
            println(calledOn.tpe.widen.toString())
            println(calledOn.tpe)
            println(line)
            println(functionName)
            println(args)
            for (function <- functionTraverser.functions){
              if(function.name == functionName.toString() && function.scope == functionScope) {
                println("matched")
                println(args)
                //check parameters to see if they are instances we have already created
                //find their new names in the function and replace those in the instances
                //create hashmap to be able to retrieve them
                //use checkExpr on the function body
                //replace names using the hashmap

              }
            }
            updateStateIfNeeded(classInfo, instances, line, instanceScope)
            methodTraverser.traverse(app)
            for(methodCall <- methodTraverser.methodCallInfo)
              println(s"here ${methodCall(0)} ${methodCall(1)}")
            methodTraverser.methodCallInfo = ListBuffer[Array[String]]()
            //checkFunction(name, args, instances)
            (None,0)
          }
          case _ => {
            //updateStateIfNeeded(classInfo, instances, line)
            (None,0)
          }
        }
      }

      def checkFunction(name:Tree,args:List[Tree], instances:Set[Instance]): Unit ={
        println(s"checking function $name with args $args")
        val functions = functionTraverser.functions
        for(function <- functions) {
          function match{
            case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
              if(tname.toString() == name.toString()) println(s"found the correct function $function which matches $name")
            case _ =>
          }
        }
        functionTraverser.traverse(name)
        println(s"fucntions are ${functionTraverser.functions}")
        //see if there is a definition for the current function
        //if so then check the contents of the function called
      }

      def dealWithLoopContents(classInfo:ClassInfo, instances:Set[Instance], expr:Trees#Tree): Unit ={
        //deal with things defined inside the for loop (and nested for loops)
        checkExpr(classInfo, expr)
        //deal with instances already defined outside the for loop
        for(instance <- instances){
          var listOfIntermediateStates = ListBuffer[Set[State]]()
          do{
            listOfIntermediateStates += instance.currentStates
            for(line <- expr) processLine(line, Set(instance), classInfo)
          } while(!listOfIntermediateStates.contains(instance.currentStates))
          for(setOfStates <- listOfIntermediateStates)
            instance.currentStates = instance.currentStates ++ setOfStates
        }
      }

      /** For a given line of code, checks if it is a method on an instance with protocol and if so updates its state
       *
       * @param classInfo
       * @param instances
       * @param line
       */
      def updateStateIfNeeded(classInfo:ClassInfo, instances: Set[compilerPlugin.Instance], line:Trees#Tree, scope:String): Unit ={
        println(s"Line inside the update state function is $line")
        val methodToStateIndices = classInfo.methodToIndices
        val className = classInfo.className
        line match{
          case app@Apply(fun, args) => {
            methodTraverser.traverse(app)
          }
          case _ =>
        }
        val applies = methodTraverser.methodCallInfo
        for(apply <- applies){
          val methodName = apply(0)
          val objectName = apply(1)
          for (instance <- instances) {
            breakable {
              if (instance.name == objectName) {
                instance.scope = scope
                var newSetOfStates:Set[State] = Set()
                for(state <- instance.currentStates) {
                  if (state.index == -2) break
                  if (methodToStateIndices.contains(methodName)) {
                    val indexSet = methodToStateIndices(methodName)
                    val newState = classInfo.transitions(state.index)(indexSet.head)
                    if (newState.name == Undefined)
                      throw new Exception(s"Invalid transition in object $objectName of type $className " +
                        s"from state(s) ${instance.currentStates} with method $methodName")
                    newSetOfStates += newState
                  }
                  else instance.currentStates = Set(State("unknown", -2))
                }
                instance.currentStates = newSetOfStates
              }
            }
          }
        }
        //reset the traverser's list to be empty
        methodTraverser.methodCallInfo = ListBuffer[Array[String]]()
      }

      /** Traverses a tree and collects (methodName, objectName) from method application statements
       *
       */
      object methodTraverser extends Traverser {
        var methodCallInfo = ListBuffer[Array[String]]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case app@Apply(fun, args) =>
              app match {
                case q"$expr(...$exprss)" =>
                  expr match {
                    case select@Select(qualifier, name) => {
                      methodCallInfo +=
                        Array(name.toString().appendedAll(getParametersFromTree(exprss)),
                          qualifier.symbol.name.toString)
                    }
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

      /** Gathers function definitions in a tree inside "functions" */
      object functionTraverser extends Traverser{
        var functions = ListBuffer[Function]()
        var scopes = mutable.Stack[String]()
        var linesUntilReturn = ListBuffer[Int]()
        override def traverse(tree: Tree): Unit = {
          if(linesUntilReturn.contains(0)) scopes.pop()
          for(x <- linesUntilReturn.indices) if(linesUntilReturn(x) > -1) linesUntilReturn(x) -= 1
          linesUntilReturn -= -1
          tree match {
            case  func@q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
              var nbLinesInExpr =0
              var nbLinesInFunc = 0
              for(line <- expr) nbLinesInExpr += 1
              for(line <- func) nbLinesInFunc += 1
              val differenceInLines = nbLinesInFunc - nbLinesInExpr
              for(x <- linesUntilReturn.indices) if(linesUntilReturn(x) > -1) linesUntilReturn(x) -= differenceInLines
              if(scopes.isEmpty) scopes.push(func.symbol.fullNameString)
              else scopes.push(s"${scopes.head}.$tname")
              functions += Function(tname.toString(), expr, scopes.head)
              linesUntilReturn += nbLinesInExpr -1
              println(linesUntilReturn)
              super.traverse(expr)
            case _ =>
              super.traverse(tree)
          }
        }
      }

      def getParametersFromTree(params:List[List[Tree]]): String={
        params match{
          case List(List()) => "()"
          case List(List(value)) => keepOnlyMethodName(value.tpe.toString()).mkString("(","",")")
          case List(values) => {
            var parameters:ArrayBuffer[String] = ArrayBuffer()
            for(elem <- values){
              parameters += keepOnlyMethodName(elem.tpe.toString)
            }
            parameters.mkString("(",",",")")
          }
          case _ => ""
        }
      }

      def keepOnlyMethodName(method:String): String ={
        if(method.contains('(')) method.substring(0,method.indexOf('('))
        else method
      }

      /** Prints something easy to see while debugging, use above an interesting print statement */
      def printBanner(): Unit ={
        println("------------------")
        println("LOOK HERE")
        println("------------------")
      }

      /** Just a dummy function to check an object's type */
      def ckType(s:String): Unit ={
        println("hi")
      }

      /** Checks that methods in return values are a subset of those in stats
       *
       * @param returnValuesArray
       * @param stats
       * @param className
       * @param filename
       */
      def checkMethodsAreSubset(returnValuesArray:Array[ReturnValue], stats: Seq[Trees#Tree], className:String, filename:String): Unit ={
        val classMethodSignatures = getMethodNames(stats)
        println(s"\n$classMethodSignatures")
        var protocolMethodSignatures: Set[String] = Set()
        for(i <- returnValuesArray.indices){
          protocolMethodSignatures += stripReturnValue(returnValuesArray(i).parentMethod.name.replaceAll("\\s", ""))
        }
        println(protocolMethodSignatures)
        if(!(protocolMethodSignatures subsetOf classMethodSignatures)) throw new Exception(
          s"Methods $protocolMethodSignatures defined in $filename are not a subset of methods $classMethodSignatures defined in class $className")
      }

      /** Gets rid of the return value in a method name string and keeps the parenthesis at the end */
      def stripReturnValue(methodName:String): String ={
        if(!(methodName.contains(':') || methodName.contains("()"))) methodName+"()"
        else if(methodName.contains(':') && !methodName.contains("(") && !methodName.contains(")")) methodName.substring(0,methodName.indexOf(':'))+"()"
        else if(methodName(methodName.length-1) == ')') methodName
        else methodName.substring(0,methodName.indexOf(')')+1)
      }

      /** Checks if an annotation is a Typestate annotation and returns the filename if so
       *
       * @param annotation
       * @return
       */
      def getFilenameFromTypestateAnnotation(annotation: AnnotationInfo):Option[String] ={
        annotation match{
          case AnnotationInfo(arg1, arg2, arg3) =>
            if(arg1.toString == "Typestate" || arg1.toString == "compilerPlugin.Typestate") {
              Some(arg2.head.toString())
            }
            else None
          case _ => None
        }
      }

      /** Returns a set of method names (as name(parameters)) from the body of a class
       *
       * @param stats
       * @return
       */
      def getMethodNames(stats: Seq[Trees#Tree]): Set[String]={
        var methodNames: Set[String] = Set()
        for(method <- stats){
          method match{
            case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" => {
              val parameters = getParameters(paramss) //HIGHLIGHTING IS WRONG HERE
              methodNames += tname+s"($parameters)"
            }
            case _ =>
          }
        }
        methodNames
      }

      /** Returns a string of the parameters given
       *
       * @param params
       * @return
       */
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

      /** Creates an sbt project, copies in the dsl and the user protocol and executes it, giving serialized data in a the project folder*/
      def executeFile(filename:String): Unit ={
        println(filename)
        s"executeUserProtocol.bat $filename".!
      }

      /** Removes protocolDir from the project */
      def cleanProject(): Unit ={
        s"cleanUp.bat".!
      }

      /** Returns protocol data from a file */
      def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
        val ois = new ObjectInputStream(new FileInputStream(filename))
        val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
        ois.close
        val file = new File(filename)
        file.delete
        stock
      }



      /** Gathers constructor instances in a tree inside "constructors" */
      object constructorTraverser extends Traverser{
        var constructors = ListBuffer[ValDef]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case valdef@ValDef(mods, name, tpt, rhs) =>
              println(show(valdef))
              rhs match {
                case app@Apply(fun, args) =>
                  fun match {
                    case select@Select(qualifier, name) =>
                      name match {
                        case termNames.CONSTRUCTOR => constructors += valdef
                        case _ =>
                      }
                      super.traverse(qualifier)
                    case _ =>

                      super.traverse(fun)
                  }
                  super.traverseTrees(args)
                case _ =>
                  super.traverse(rhs)
              }
              super.traverse(tpt)
            case _ =>
              super.traverse(tree)
          }
        }
      }
    }
}