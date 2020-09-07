package compilerPlugin

import java.io.{File, FileInputStream, ObjectInputStream}
import java.nio.file.{Files, Paths}

import ProtocolDSL.{ReturnValue, State}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees
import scala.sys.process._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}
import scala.util.control.Breaks._

case class Instance(var className: String, var name:String, var currentStates:Set[State], var scope: mutable.Stack[String]){
  def updateState(stateToRemove:State, stateToAdd:State): Unit ={
    this.currentStates -= stateToRemove
    this.currentStates += stateToAdd
  }
  override def toString(): String={
    this.className + " " + this.name +" "+ this.currentStates+" "+scope.reverse.mkString(".")
  }
  override def equals(instance:Any): Boolean ={
    instance match{
      case i:Instance => i.name.equals(name) && i.canEqual(this) && i.scope.equals(scope) && i.className.equals(className)
      case _ => false
    }
  }

  override def hashCode():Int={
    name.hashCode + className.hashCode + scope.hashCode
  }
}

case class ClassInfo(className:String, transitions:Array[Array[State]], states:Array[State],
                     methodToIndices:mutable.HashMap[String, Set[Int]], isObject:Boolean=false){
  override def toString(): String={
    this.className + " " + transitions.foreach(_.mkString(", ")) + " " + states.mkString(", ") + " " + methodToIndices + " " + isObject
  }
}

class GetFileFromAnnotation(val global: Global) extends Plugin {
  val name = "GetFileFromAnnotation"
  val description = "Checks the protocol defined on a class or object with the Typestate annotation"
  lazy val components =
    new MyComponent(global) :: Nil
}

class MyComponent(val global: Global) extends PluginComponent {
  import global._
  case class Function(name: String, params: ArrayBuffer[Array[String]], body: Tree, scope: String) {
    override def toString(): String = {
      this.name + " " + this.params + " " + this.scope
    }
  }
  val runsAfter: List[String] = List[String]("refchecks")
  val phaseName: String = "compilerPlugin.GetFileFromAnnotation.this.name"
  def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

    class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
      var compilationUnit:CompilationUnit=_
      var currentScope:mutable.Stack[String] = mutable.Stack()
      val Undefined = "_Undefined_"
      var classInfo: ClassInfo=_
      override def name: String = "compilerPlugin.GetFileFromAnnotation.this.name"

      def getType[T: TypeTag](obj: T) = typeOf[T]

      def apply(unit: CompilationUnit): Unit = {
        classAndObjectTraverser.traverse(unit.body)
        println(classAndObjectTraverser.classesAndObjects)
        functionTraverser.traverse(unit.body)
        constructorTraverser.traverse(unit.body)
        println(constructorTraverser.constructors)
        this.compilationUnit = unit
        var setOfClassesWithProtocols: Set[String] = Set()
        for (tree@q"$mods class $className[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- unit.body) {
          checkElement(body, getScope(tree)+s".${className.toString()}", tree) match{
            case Some(className) => setOfClassesWithProtocols += className
            case None =>
          }
        }
        for(tree@q"$mods object $objectName extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- unit.body){
          checkElement(body, objectName.toString, tree, true) match{
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
              this.classInfo = ClassInfo(name, transitions, states, methodToIndices, isObject)
              println("checking class "+classInfo.className)
              checkClassIsUsedCorrectly()
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
       * Limited at the moment
       *
       * Only works with code inside "App" and main
       * Only works on a single file
       *
       * Does not take into account linearity
       * Does not deal with objects returned by a function
       * Does not deal with try-catch
       * Does not deal with if on its own
       * */
      def checkClassIsUsedCorrectly(): Unit ={
        for(line@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body){
          for(parent <- parents){
            if(parent.toString() == "App") {
              currentScope.push(line.symbol.owner.fullName)
              currentScope.push(tname.toString())
              checkInsideAppBody(body)
            }
          }
          for (definition <- body){
            definition match{
              case line@q"$mods def main[..$tparams](...$paramss): $tpt = $expr" =>
                currentScope.push(line.symbol.owner.fullName)
                currentScope.push("main")
                if(getParameters(paramss) == "Array[String]") checkInsideMainBody(expr)
              case _ =>
            }
          }
        }
      }

      /** Goes inside "App" object to see if there are instances with protocols and if they are following their protocol
       *
       * @param code
       */
      def checkInsideAppBody(code:Seq[Trees#Tree], givenInstances:Set[Instance]=Set()): Unit = {
        var instances: Set[Instance] = givenInstances
        if(classInfo.isObject) instances +=Instance(classInfo.className, classInfo.className, Set(classInfo.states(0)), currentScope.clone())
        for (line <- code) {
            val newInstanceAndNbLinesToSkip = processLine(line, instances)
            instances = newInstanceAndNbLinesToSkip._1
        }
        println("\nInstances:")
        instances.foreach(println)
      }

      def checkInsideMainBody(code:Trees#Tree, givenInstances:Set[Instance]=Set()): Set[Instance] ={
        var instances: Set[Instance] = Set()
        for(instance <- givenInstances) instances += Instance(instance.className, instance.name, instance.currentStates, instance.scope)
        if(classInfo.isObject) instances += Instance(classInfo.className, classInfo.className, Set(classInfo.states(0)), currentScope.clone())
        var nbOfLinesToSkip = 0
        for (line <- code) {
          breakable {
            if(nbOfLinesToSkip>0){
              nbOfLinesToSkip-=1
              break
            }
            val newInstanceAndNbLinesToSkip = processLine(line, instances)
            instances = newInstanceAndNbLinesToSkip._1
            nbOfLinesToSkip = newInstanceAndNbLinesToSkip._2
          }
        }
        println("\nInstances:")
        instances.foreach(println)
        instances
      }

      def processLine(line:Trees#Tree, instances: Set[Instance]): (Set[Instance], Int) ={
        val className = classInfo.className
        val states = classInfo.states
        println("checking line "+line+" at line number "+line.pos.line)
        println(showRaw(line))
        line match {
          case q"$mods val $tname: $tpt = new $classNm(...$exprss)" =>
            if (getScope(classNm) + s".${classNm.toString()}" == className) {
              var newInstances = for(instance <- instances) yield instance
              for(newInstance <- newInstances if(newInstance == Instance(className, tname.toString(), Set(states(0)),currentScope.clone())))
                newInstances -= newInstance
              newInstances += Instance(className, tname.toString(), Set(states(0)),currentScope.clone())
              (newInstances,0)
            } else (instances,0)
          case q"$mods var $tname: $tpt = new $classNm(...$exprss)" =>
            if (getScope(classNm)  == className) {
              var newInstances = for(instance <- instances) yield instance
              for(newInstance <- newInstances if(newInstance == Instance(className, tname.toString(), Set(states(0)),currentScope.clone())))
                newInstances -= newInstance
              newInstances += Instance(className, tname.toString(), Set(states(0)),currentScope.clone())
              (newInstances,0)
            } else (instances,0)
          case q"for (..$enums) $expr" => {
            val newInstances = dealWithLoopContents(instances, expr)
            (newInstances, getLengthOfTree(line)-1) //-1 because we are processing the current one already
          }
            //while(true) and dowhile(true)
          case LabelDef(TermName(name), List(), block @ Block(statements, Apply(Ident(TermName(name2)), List())))
            if (name.startsWith("while$") || name.startsWith("doWhile$")) && name2 == name =>{
            val newInstances = dealWithLoopContents(instances, block.asInstanceOf[Trees#Tree])
            (newInstances, getLengthOfTree(line)-1) //-1 because we are processing the current one already
          }
          case q"while ($cond) $expr" =>{
            val newInstances = dealWithLoopContents(instances, expr)
            (newInstances, getLengthOfTree(line)-1) //-1 because we are processing the current one already
          }
          case q"do $expr while ($cond)" =>{
            val newInstances = dealWithLoopContents(instances, expr)
            checkInsideMainBody(expr, instances)
            (newInstances, 0)
          }
          case q"for (..$enums) yield $expr" =>{
            val newInstances = dealWithLoopContents(instances, expr)
            (newInstances, getLengthOfTree(line)-1) //-1 because we are processing the current one already
          }
          case func@Apply(Ident(functionName), args) =>{
            println("found function in weird "+func)
            val newInstances = dealWithFunction(func, functionName, args, instances)
            val updatedInstances = updateStateIfNeeded(newInstances, line)
            (updatedInstances, getLengthOfTree(line)-1) //because we are processing the current one already
          }
          case func@Apply(Select(instanceCalledOn, functionName), args) =>{
            println("found function "+func)
            val newInstances = dealWithFunction(func, functionName, args, instances)
            val updatedInstances = updateStateIfNeeded(newInstances, line)
            println("after being returned by update and to be returned by process line instances are "+ updatedInstances)
            (updatedInstances, getLengthOfTree(line)-1) //because we are processing the current one already
          }
          case func@q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" => {
            (instances, getLengthOfTree(line)-1) //because we are processing the current one already
          }
          case func@q"(..$params) => $expr" =>
            (instances, 0)
          case q"if ($cond) $ifBody else $elseBody" =>
            var ifInstances:Set[Instance] = Set()
            for(instance <- instances) ifInstances += Instance(instance.className, instance.name, instance.currentStates, instance.scope)
            ifInstances = checkInsideMainBody(ifBody, ifInstances)
            var elseInstances:Set[Instance] = Set()
            for(instance <- instances) elseInstances += Instance(instance.className, instance.name, instance.currentStates, instance.scope)
            elseInstances = checkInsideMainBody(elseBody, elseInstances)
            val resultingInstances = mergeInstanceStates(ifInstances, elseInstances)
            (resultingInstances,getLengthOfTree(line)-1)

          case _ => {
            (instances,0)
          }
        }
      }

      def mergeInstanceStates(firstInstances:Set[Instance], secondInstances:Set[Instance]): Set[Instance] ={
        var mergedInstances:Set[Instance] = Set()
        for(firstInstance <- firstInstances){
          secondInstances.find(instance => instance == firstInstance) match{
            case Some(instance) =>
              mergedInstances += Instance(firstInstance.className, firstInstance.name,
                firstInstance.currentStates ++ instance.currentStates, firstInstance.scope)
            case None => mergedInstances += firstInstance
          }
        }
        for(secondInstance <- secondInstances) if(!firstInstances.contains(secondInstance)){
          mergedInstances += secondInstance
        }
        mergedInstances
      }

      def getClosestScopeInstance(name:String, instances:Set[Instance]): Option[Instance] ={
        if(instances.isEmpty) return None
        var closestScope:mutable.Stack[String] = mutable.Stack()
        var closestInstance = Instance("class","dummyInstance", Set(), mutable.Stack())
        var foundInstance = false
        for(instance <- instances) {
          if(instance.name == name){
            if(closestScope.size < instance.scope.size) {
              closestScope = instance.scope
              closestInstance = instance
              foundInstance = true
            }
          }
        }
        if(foundInstance) return Some(closestInstance)
        None
      }

      def dealWithFunction(funcCall: global.Apply, functionName: global.Name, args: List[global.Tree], instances:Set[Instance]):Set[Instance] = {
        println("found function call "+funcCall)
        //finding function definition
        val functionScope = getScope(funcCall, true)
        for (function <- functionTraverser.functions){
          if(function.name == functionName.toString() && function.scope == functionScope) {
            println("matched functions, found body "+function.body)
            //handling parameters
            var paramNameToInstanceName = new mutable.HashMap[String, String]
            var argCounter = 0
            for(arg <- args){
              var argString = arg.toString()
              if(argString.contains(".")) argString = argString.substring(argString.lastIndexOf(".")+1)
              getClosestScopeInstance(argString,instances) match{
                case Some(instance) =>
                  val paramName = function.params(argCounter)(0)
                  paramNameToInstanceName += paramName -> instance.name
                  instance.name = paramName
                case None =>
              }
              argCounter += 1
            }
            //checking inside the function body
            currentScope.push(functionName.toString())
            val newInstances = checkInsideMainBody(function.body, instances)
            currentScope.pop()
            //renaming parameters
            for(mapping <- paramNameToInstanceName) {
              for(instance <- instances if instance.name == mapping._1) instance.name = mapping._2
            }
            return newInstances
          }
        }
        instances
      }

      def dealWithLoopContents(instances:Set[Instance], expr:Trees#Tree): Set[Instance] ={
        var newInstances = for(instance <- instances) yield instance
        var instanceToInterimStates: mutable.HashMap[Instance, ListBuffer[Set[State]]] = mutable.HashMap()
        for(instance <- newInstances) instanceToInterimStates += instance -> ListBuffer()
        do{
          for(instance <- newInstances if instanceToInterimStates.contains(instance))
            instanceToInterimStates(instance) += instance.currentStates
          for(line <- expr) {
            newInstances = processLine(line, newInstances)._1
            for(updatedInstance <- newInstances if instanceToInterimStates.contains(updatedInstance)) {
              instanceToInterimStates(updatedInstance)(instanceToInterimStates(updatedInstance).length - 1) = updatedInstance.currentStates
            }
          }
        } while(!duplicatesInListsOf(instanceToInterimStates))
        for(instance <- newInstances if instanceToInterimStates.contains(instance)){
          for(setOfStates <- instanceToInterimStates(instance))
            instance.currentStates = instance.currentStates ++ setOfStates
        }
        newInstances
      }

      def duplicatesInListsOf(map:mutable.HashMap[Instance, ListBuffer[Set[State]]]):Boolean={
        for((instance, list) <- map) for((instance, list) <- map if list.diff(list.distinct).nonEmpty) return true
        false
      }

      /** For a given line of code, checks if it is a method on an instance with protocol and if so updates its state
       *
       * @param instances
       * @param line
       */
      def updateStateIfNeeded(instances: Set[compilerPlugin.Instance], line:Trees#Tree): Set[Instance] ={
        println("inside update state for line "+line)
        val methodToStateIndices = classInfo.methodToIndices
        val className = classInfo.className
        line match{
          case app@Apply(fun, args) => {
            methodTraverser.traverse(app)
          }
          case _ =>
        }
        val methodCallInfos = methodTraverser.methodCallInfos
        for(methodCallInfo <- methodCallInfos){
          val methodName = methodCallInfo(0)
          val instanceName = methodCallInfo(1)
          println("instances inside update are "+instances)
          println("instance name to find within those is "+instanceName)
          getClosestScopeInstance(instanceName, instances) match{
            case Some(instance) =>
              println("got instance"+instance)
              println("it has "+ instance.currentStates)
              println("method to indices is "+classInfo.methodToIndices)
              breakable {
                var newSetOfStates:Set[State] = Set()
                for(state <- instance.currentStates) {
                  if (state.index == -2) break
                  if (methodToStateIndices.contains(methodName)) {
                    println("found method name "+methodName)
                    val indexSet = methodToStateIndices(methodName)
                    println("index set is "+indexSet)
                    var newStates:Set[State] = Set[State]()
                    newStates += classInfo.transitions(state.index)(indexSet.min)
                    if(indexSet.size > 1 && classInfo.transitions(state.index)(indexSet.min).name == Undefined)
                        newStates = for(x <- indexSet - indexSet.min) yield classInfo.transitions(state.index)(x)
                    println("new states are "+newStates)
                    for(state <- newStates if state.name == Undefined) {
                      throw new Exception(s"Invalid transition in instance $instanceName of type $className " +
                        s"from state(s) ${instance.currentStates} with method $methodName " +
                        s"in file ${line.pos.source} at line ${line.pos.line}")
                    }
                    newSetOfStates = newSetOfStates ++ newStates
                  }
                  else instance.currentStates = Set(State("unknown", -2))
                }
                instance.currentStates = newSetOfStates
              }
            case None =>
          }
        }
        //reset the traverser's list to be empty
        methodTraverser.methodCallInfos = ListBuffer[Array[String]]()
        println("instances at the end of update if needed are "+instances)
        instances
      }

      /** Traverses a tree and collects (methodName, instanceName) from method application statements
       *
       */
      object methodTraverser extends Traverser {
        var methodCallInfos = ListBuffer[Array[String]]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case app@Apply(fun, args) =>
              app match {
                case q"$expr(...$exprss)" =>
                  expr match {
                    case select@Select(qualifier, name) => {
                      var instanceName = qualifier.toString()
                      if(qualifier.hasSymbolField) instanceName = qualifier.symbol.name.toString
                      println("method name is "+name.toString().appendedAll(getParametersFromTree(exprss)))
                      println("instance name is "+instanceName)
                      methodCallInfos +=
                        Array(name.toString().appendedAll(getParametersFromTree(exprss)),
                          instanceName)
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

      case class ClassOrObject(name:String, body:Seq[Trees#Tree], scope:String, isObject:Boolean=false, met:Boolean=false){
        override def toString(): String={ s"$name $scope" }
      }

      object classAndObjectTraverser extends Traverser{
        var classesAndObjects = ListBuffer[ClassOrObject]()
        override def traverse(tree: Tree): Unit = {
          tree match{
            case obj@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
              classesAndObjects += ClassOrObject(tname.toString(), body, getScope(obj), true)
              super.traverse(obj)
            case cla@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
              classesAndObjects += ClassOrObject(tpname.toString(), stats, getScope(cla))
              super.traverse(cla)
            case _ =>
              super.traverse(tree)
          }
        }
      }

      /** Gathers function definitions in a tree inside "functions" */
      object functionTraverser extends Traverser{
        var functions = ListBuffer[Function]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case  func@q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
              val parameters = getParametersWithInstanceNames(paramss)
              if(tname.toString() != "<init>")
                functions += Function(tname.toString(), parameters, expr, getScope(func))
              super.traverse(expr)
            case _ =>
              super.traverse(tree)
          }
        }
      }

      def getScope(obj:Tree, dontCheckSymbolField:Boolean = false): String ={
        var objectScope = ""
        if(obj.hasSymbolField || dontCheckSymbolField) {
          for (symbol <- obj.symbol.owner.ownerChain.reverse)
            objectScope += symbol.name + "."
          objectScope = objectScope.substring(0, objectScope.lastIndexOf('.'))
        }
        objectScope
      }

      def getLengthOfTree(tree:Trees#Tree): Int ={
        var length = 0
        for(line <- tree) length +=1
        length
      }

      def getScopeString(scopeStack:mutable.Stack[String]): String ={
        scopeStack.reverse.mkString(".")
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
          case List(List(value)) => {
            println(value.name.toString())
            value.tpt.toString()
          }
          case List(values) => {
            var parameters:ArrayBuffer[String] = ArrayBuffer()
            for(elem <- values){
              println(elem.name.toString())
              parameters += elem.tpt.toString
            }
            parameters.mkString(",")
          }
          case _ => ""
        }
      }

      def getParametersWithInstanceNames(params:List[List[ValDef]]): ArrayBuffer[Array[String]] ={
        params match{
          case List(List()) => ArrayBuffer(Array(""))
          case List(List(value)) => {
            value.tpt.toString()
            ArrayBuffer(Array(value.name.toString(), value.tpt.toString()))
          }
          case List(values) => {
            var parameters:ArrayBuffer[Array[String]] = ArrayBuffer()
            for(elem <- values){
              parameters += Array(elem.name.toString(), elem.tpt.toString)
            }
            parameters
          }
          case _ => ArrayBuffer()
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

      //constructor@ValDef(mods, name, tpt, Apply(Select(qualifier, termNames.CONSTRUCTOR), args))

      /** Gathers constructor instances in a tree inside "constructors" */
      object constructorTraverser extends Traverser{
        var constructors = ListBuffer[ValDef]()
        override def traverse(tree: Tree): Unit = {
          tree match {
            case valdef@ValDef(mods, name, tpt, rhs) =>
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