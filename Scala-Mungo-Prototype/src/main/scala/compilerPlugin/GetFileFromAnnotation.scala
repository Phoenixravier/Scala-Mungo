package compilerPlugin

import java.nio.file.{Files, Paths}

import ProtocolDSL.{ReturnValue, State}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}
import scala.util.control.Breaks._
import Util._


/** My plugin */
class GetFileFromAnnotation(val global: Global) extends Plugin {
  val name = "GetFileFromAnnotation"
  val description = "Checks the protocol defined on a class or object with the Typestate annotation"
  lazy val components =
    new MyComponent(global) :: Nil
}

/** The component which will run when my plugin is used */
class MyComponent(val global: Global) extends PluginComponent {
  import global._

  case class Function(name: String, params: ArrayBuffer[Array[String]],
                      returnType: Trees#Tree, body: Trees#Tree, scope: mutable.Stack[String],
                      var stateCache: Map[ArrayBuffer[(String, Set[String], Set[State])], ArrayBuffer[Set[State]]],
                      var returned: Option[Set[Instance]]) { //might replace string with elementInfo for more precision (on build)
    override def toString: String = {
      s"name: $name parameters: $params return type: $returnType scope: $scope body: $body stateCache: $stateCache returned: $returned"
    }
  }

  val runsAfter: List[String] = List[String]("refchecks")
  val phaseName: String = "compilerPlugin.GetFileFromAnnotation.this.name"

  def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

  /** Phase which is ran by the plugin */
  class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
    var compilationUnit: CompilationUnit = _
    var currentElementInfo: ElementInfo = _
    var savedBreakInstances: mutable.Map[String, ArrayBuffer[Set[Instance]]] = mutable.Map[String, ArrayBuffer[Set[Instance]]]()

    override def name: String = "compilerPlugin.GetFileFromAnnotation.this.name"

    /** Entry point of the plugin. Goes through the code collecting object, class and function bodies, then checks
     *  the code for protocol violations.
     *  Goes through apply function once for each file.
     *
     * @param unit : contains tree of the code in body
     */
    def apply(unit: CompilationUnit): Unit = {
      ckType("happy")
      println("at top of apply, currElmInfo is "+currentElementInfo)
      println("hello, plugin is running")
      //println(s"whole source is: \n ${unit.body}")
      //println("raw is: "+showRaw(unit.body))
      compilationUnit = unit
      //find all the classes, objects and functions in the code so we can jump to them later
      functionTraverser.traverse(unit.body)
      classAndObjectTraverser.traverse(unit.body)
      checkCode()
    }

    /** Finds objects and classes with protocols and goes through the code checking that the protocols are followed
     *
     */
    def checkCode(): Unit = {
      var setOfClassesWithProtocols: Set[String] = Set()
      for (tree@q"$mods class $className[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
        checkElement(body, className.toString(), getScope(tree), tree) match {
          case Some(className) => setOfClassesWithProtocols += className
          case None =>
        }
      }
      for (tree@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
        val objectName = mutable.Stack[String]()
        objectName.push(name.toString())
        checkElement(body, name.toString(), objectName, tree, isObject = true) match {
          case Some(objectName) => setOfClassesWithProtocols += objectName
          case None =>
        }
      }
    }

    /** Checks whether the object or class is following its protocol in the code.
     * It first checks if the element has a typestate annotation, then runs the protocol and collects the information
     * from it.
     * Then it checks the methods in the protocol are a subset of those defined in the element.
     * Then it checks the protocol is followed
     *
     * @param body     The code to check
     * @param name     Name of the element
     * @param tree     The entire code
     * @param isObject Whether or not the element to check is an Object (as opposed to a Class)
     * @return
     */
    def checkElement(body: Seq[Trees#Tree], name: String, scope: mutable.Stack[String], tree: Tree, isObject: Boolean = false): Option[String] = {
      val annotations = tree.symbol.annotations
      for (annotation@AnnotationInfo(arg1, arg2, arg3) <- annotations) {
        getFilenameFromTypestateAnnotation(annotation) match {
          case Some(filename) => //a correct Typestate annotation is being used
            //execute the DSL in the protocol file and serialize the data into a file
            println("filename is "+filename)
            executeFile(filename)
            //retrieve the serialized data
            if (!Files.exists(Paths.get("protocolClasses\\EncodedData.ser")))
              throw new badlyDefinedProtocolException(s"The protocol at $filename could not be processed, " +
                s"check you have an end statement at the end of the protocol and that the name of the file is " +
                s"the same as the name of the protocol and that the path given for the protocol is correct")
            val (stateMachine, states, returnValuesArray) = Util.getDataFromFile("protocolClasses\\EncodedData.ser")
            //println("stateMachine are "+stateMachine.mkString("Array(", ", ", ")"))
            println("states are "+states.mkString("Array(", ", ", ")"))
            println("ret values are "+returnValuesArray.mkString("Array(", ", ", ")"))
            checkProtocolMethodsSubsetClassMethods(returnValuesArray, body, name, filename)
            val methodToIndices = Util.createMethodToIndicesMap(returnValuesArray)
            val returnValueToIndice = Util.createReturnValueToIndiceMap(returnValuesArray)
            val stateToAvailableMethods = Util.createStateToAvailableMethodsMap(returnValuesArray)
            println("state to available methods: "+stateToAvailableMethods)
            currentElementInfo = ElementInfo(name, scope, stateMachine, states, methodToIndices, returnValueToIndice, isObject)
            println("after being set, cei is "+currentElementInfo)
            println("checking class " + currentElementInfo.name)
            checkElementIsUsedCorrectly()
            Some(name)
          case None =>
            if(currentElementInfo != null) {
              println("going through class "+currentElementInfo.name)
              checkElementIsUsedCorrectly()
            }
        }
      }
      None
    }


    /** Checks that a class or object is following its protocol
     * Goes through the code to find either the object with App or the main function and thereby gets the entrypoint
     * of the code and can start analysing it from there.
     *
     * Limited at the moment
     *
     * */
    def checkElementIsUsedCorrectly(): Unit = {
      for (line@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
        breakable {
          for (parent <- parents) {
            if (parent.toString() == "App") {
              Util.currentScope = getScope(line)
              Util.currentScope.push(tname.toString())
              checkInsideObjectBody(body)
              break
            }
          }
          for (definition <- body) {
            definition match {
              case mainLine@q"$mods def main[..$tparams](...$paramss): $tpt = $expr" =>
                Util.currentScope = getScope(line)
                /*_*/
                if (getParameters(paramss) == "Array[String]") {
                  /*_*/
                  val instances = checkObject(tname.toString())
                  Util.currentScope.push(tname.toString())
                  Util.currentScope.push("main")
                  checkInsideFunctionBody(expr, instances)
                }
              case _ =>
            }
          }
        }
      }
    }

    /** Goes inside "App" object to see if there are instances with protocols and if they are following their protocol
     * Analyses the code line by line with checkInsideFunctionBody. Takes instances or creates a new set of them and returns
     * updated ones
     *
     * @param code The code to check
     */
    def checkInsideObjectBody(code: Seq[Trees#Tree], givenInstances: Set[Instance] = Set()): Set[Instance] = {
      var newInstances = for (instance <- givenInstances) yield instance
      if (currentElementInfo.isObject && !currentElementInfo.isAssigned) {
        newInstances += Instance(currentElementInfo.name, Set(Alias(currentElementInfo.name, Util.currentScope.clone)), Set(currentElementInfo.states(0)))
        currentElementInfo.isAssigned = true
      }
      for(line <- code) {
        newInstances = checkInsideFunctionBody(line, newInstances)._1
      }
      println("\nInstances:")
      newInstances.foreach(println)
      newInstances
    }


    /** Checks the code inside the body of a function for protocol violations.
     * Goes line by line and skips lines if needed (for example when at a definition or a loop).
     *
     * @param code           The code to check
     * @param givenInstances Set of instances given to be updated (optional). Will create a new set if not
     * @return
     */
    def checkInsideFunctionBody(code: Trees#Tree, givenInstances: Set[Instance] = Set()): (Set[Instance], Option[Set[Instance]]) = {
      var instances = for (instance <- givenInstances) yield instance
      if (currentElementInfo.isObject && !currentElementInfo.isAssigned) {
        instances += Instance(currentElementInfo.name, Set(Alias(currentElementInfo.name, Util.currentScope.clone)), Set(currentElementInfo.states(0)))
        currentElementInfo.isAssigned = true
      }
      var returned: Option[Set[Instance]] = None
      var nbOfLinesToSkip = 0
      for (line <- code) {
        breakable {
          if (nbOfLinesToSkip > 0) {
            nbOfLinesToSkip -= 1
            break
          }
          val newInstanceAndNbLinesToSkipAndReturned = processLine(line, instances)
          instances = newInstanceAndNbLinesToSkipAndReturned._1
          nbOfLinesToSkip = newInstanceAndNbLinesToSkipAndReturned._2
          if(nbOfLinesToSkip == -1)
              return (Set(), None)
          if (newInstanceAndNbLinesToSkipAndReturned._3.isDefined) // do this to avoid erasing returned element
            returned = newInstanceAndNbLinesToSkipAndReturned._3
        }
      }
      println("\nInstances:")
      instances.foreach(println)
      (instances, returned)
    }



    /** Checks a line and returns possibly updated instances.
     * Has different cases for different types of line
     *
     * @param line      line of code to analyse
     * @param instances instances to update
     * @return
     */
    def processLine(line: Trees#Tree, instances: Set[Instance]): (Set[Instance], Int, Option[Set[Instance]]) = {
      println(s"checking line at line number " + line.pos.line)
      println(s"instances before checking line are $instances")
      //println(showRaw(line))
      line match {
        case Apply(Select(Select(scope, label), TermName("break")), body) =>
          dealWithBreak(instances, label.toString())
          (Set(), -1, None)
        case Apply(Select(Ident(TermName(label)), TermName("break")), List()) =>
          dealWithBreak(instances, label)
          (Set(), -1, None)
        case Apply(Select(Select(scope, label), TermName("breakable")), body) =>
          val newInstances = dealWithBreakable(instances, label, body)
          (newInstances, Util.getLengthOfTree(line) - 1, None)
        case Apply(Select(Ident(label), TermName("breakable")), body) =>
          val newInstances = dealWithBreakable(instances, label, body)
          (newInstances, Util.getLengthOfTree(line) - 1, None)
        //definitions to skip over (object, class, function)
        case q"$modifs object $tname extends { ..$earlydefins } with ..$pparents { $sself => ..$body }" =>
          (instances, Util.getLengthOfTree(line) - 1, None)
        case q"$mod class $pname[..$tpara] $actMods(...$para) extends { ..$defs } with ..$prnts { $self => ..$sts }" =>
          (instances, Util.getLengthOfTree(line) - 1, None)
        case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" =>
          (instances, Util.getLengthOfTree(line) - 1, None)
        //assignment
        case q"val $assignee = $newValue" =>
          val newInstances = /*_*/ processNovelAssignment(assignee.toString, newValue, instances) /*_*/
          (newInstances, Util.getLengthOfTree(line) - 1, None)
        case q"var $assignee = $newValue" =>
          val newInstances = /*_*/ processNovelAssignment(assignee.toString, newValue, instances) /*_*/
          (newInstances, Util.getLengthOfTree(line) - 1, None)
        case q"$assignee = $newValue" =>
          val newInstances = processAssignment(assignee.toString, newValue, instances)
          (newInstances, Util.getLengthOfTree(line) - 1, None)
        //for loops
        case q"for (..$enums) $expr" =>
          val newInstances = dealWithForLoop(enums, instances, expr)
          (newInstances, Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        case q"for (..$enums) yield $expr" =>
          val newInstances = dealWithForLoop(enums, instances, expr)
          (newInstances, Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        //while(true) and dowhile(true)
        case LabelDef(TermName(name), List(), block@Block(statements, Apply(Ident(TermName(name2)), List())))
          if (name.startsWith("while$") || name.startsWith("doWhile$")) && name2 == name =>
          dealWithDoWhileLoop(null, instances, block.asInstanceOf[Trees#Tree])
          (Set(), Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        //while loops
        case q"while ($cond) $loopContents" =>
          val newInstances = dealWithWhileLoop(cond, instances, loopContents)
          (newInstances, Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        case q"do $loopContents while ($cond)" =>
          val newInstances = dealWithDoWhileLoop(cond, instances, loopContents)
          (newInstances, 0, None)
        //Functions (first is for functions defined in the same scope, second the others)
        case func@Apply(Ident(functionName), args) =>
          var copiedInstances = Util.copyInstances(instances)
          val (newInstances, returned) = dealWithFunction(func, functionName, args, instances)
          val updatedInstances = updateStateIfNeeded(copiedInstances, newInstances, line)
          (updatedInstances, Util.getLengthOfTree(line) - 1, returned) //because we are processing the current one already
        case func@Apply(Select(instanceCalledOn, functionName), args) =>
          var copiedInstances = Util.copyInstances(instances)
          val (newInstances, returned) = dealWithFunction(func, functionName, args, instances, instanceCalledOn)
          val updatedInstances = updateStateIfNeeded(copiedInstances, newInstances, line)
          (updatedInstances, Util.getLengthOfTree(line) - 1, returned) //because we are processing the current one already
        case q"if ($cond) $ifBody else $elseBody" =>
          val (newInstances, returned) = dealWithIfElse(cond, ifBody, elseBody, instances)
          (newInstances, Util.getLengthOfTree(line) - 1, returned)
        case q"try $tryBody catch { case ..$cases } finally $finallyBody" =>
          val newInstances = /*_*/ checkTryCatchFinally(tryBody, cases, finallyBody, instances) /*_*/
          (newInstances, Util.getLengthOfTree(line) - 1, None)
        case q"$expr match { case ..$cases }" =>
          val newInstances = /*_*/ processMatchStatement(expr, cases, instances) /*_*/
          (newInstances, Util.getLengthOfTree(line) - 1, None)

        //All three next cases are to check for solitary object name on a line
        case Ident(TermName(objectName)) =>
          checkObject(objectName, instances)
          getClosestScopeAliasInfo(objectName, instances) match {
            case Some(aliasInfo) =>
              val returned = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (instances, 0, Some(returned))
            case None =>
              (instances, 0, None)
          }
        case Select(location, expr) =>
          var exprString = expr.toString()
          if (exprString.lastIndexOf(".") != -1)
            exprString = exprString.substring(exprString.lastIndexOf(".") + 1)
          checkObject(exprString, instances)
          getClosestScopeAliasInfo(exprString.trim, instances) match {
            case Some(aliasInfo) =>
              val returned = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (instances, 0, Some(returned))
            case None =>
              (instances, 0, None)
          }
        case Block(List(expr), Literal(Constant(()))) =>
          var exprString = expr.toString()
          if (exprString.lastIndexOf(".") != -1)
            exprString = exprString.substring(exprString.lastIndexOf(".") + 1)
          checkObject(exprString, instances)
          getClosestScopeAliasInfo(exprString.trim, instances) match {
            case Some(aliasInfo) =>
              val returned = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (instances, 0, Some(returned))
            case None =>
              (instances, 0, None)
          }
        case _ =>
          (instances, 0, None)
      }
    }

    private def dealWithBreak(instances: Set[Instance], label: String) = {
      if (savedBreakInstances.contains(label)) {
        savedBreakInstances(label) += copyInstances(instances)
      } else
        savedBreakInstances += (label -> ArrayBuffer(copyInstances(instances)))
      println(s"after dealing with break with label $label, saved instances are "+savedBreakInstances)
    }

    def dealWithBreakable(instances:Set[Instance], label:Name, body:Seq[Trees#Tree]):Set[Instance] = {
      println("In breakable with label " + label)
      var newInstances = checkInsideObjectBody(body, instances)
      println("merging instances at the end of breakable with label "+label)
      if (savedBreakInstances.contains(label.toString())) {
        for (instances <- savedBreakInstances(label.toString()))
          newInstances = mergeInstanceStates(newInstances, instances)
      }
      println("at the end of breakable, instances are " + newInstances)
      savedBreakInstances.remove(label.toString())
      newInstances
    }

    def processAssignment(assignee: String, assigned: Trees#Tree, instances: Set[Instance]): Set[Instance] = {
      val newInstancesAndReturned = checkInsideFunctionBody(assigned, instances)
      var newInstances = newInstancesAndReturned._1
      val returnedAssigned = newInstancesAndReturned._2
      getClosestScopeAliasInfo(assignee, newInstances) match {
        case Some(assigneeAliasInfo) =>
          returnedAssigned match {
            case Some(returned) =>
              var returnedInstances = Util.copyInstances(returned)
              var scopesToRemove: ArrayBuffer[mutable.Stack[String]] = ArrayBuffer()
              newInstances = removeAliases(newInstances, assigneeAliasInfo._1)
              for (instance <- returnedInstances) {
                if (instance.containsScopeAlias())
                  scopesToRemove += instance.aliases.last.scope
                else {
                  if (newInstances.contains(instance)) {
                    for (existingInstance <- newInstances) {
                      if (instance == existingInstance)
                        existingInstance.aliases += Alias(assigneeAliasInfo._1, assigneeAliasInfo._2)
                    }
                  }
                  else {
                    instance.aliases += Alias(assigneeAliasInfo._1, assigneeAliasInfo._2)
                    newInstances += instance
                  }
                }
              }
              for (scopeToRemove <- scopesToRemove) {
                newInstances = Util.removeAllAliasesInScope(newInstances, scopeToRemove)
              }
            case None =>
          }
          if (returnedAssigned != null) {

          }
        case None =>
      }
      newInstances
    }


    /** Processes a val assignee = assigned statement
     * Checks if assigned is an existing alias and if so adds assignee to its list of aliases
     *
     * @param assignee  In val/var x = y, this is x. Always comes as a new val or var.
     * @param assigned  In val/var x = y, this is y.
     * @param instances The instances passed in to process the assignment with.
     * @return
     */
    def processNovelAssignment(assignee: String, assigned: Trees#Tree, instances: Set[Instance]): Set[Instance] = {
      var (newInstances, returnedAssigned) = checkInsideFunctionBody(assigned, instances)
      returnedAssigned match {
        case Some(returned) =>
          var returnedInstances = Util.copyInstances(returned)
          var scopesToRemove: ArrayBuffer[mutable.Stack[String]] = ArrayBuffer()
          for (instance <- returnedInstances) {
            if (instance.containsScopeAlias()) {
              scopesToRemove += instance.aliases.last.scope
            } else {
              if (newInstances.contains(instance)) {
                for (existingInstance <- newInstances) {
                  if (instance == existingInstance)
                    existingInstance.aliases += Alias(assignee, Util.currentScope.clone())
                }
              }
              else {
                instance.aliases += Alias(assignee, Util.currentScope.clone())
                newInstances += instance
              }
            }
          }
          for (scopeToRemove <- scopesToRemove) {
            newInstances = Util.removeAllAliasesInScope(newInstances, scopeToRemove)
          }
        case None =>
      }
      newInstances
    }


    def isAliasCallingProtocolMethod(expr: Trees#Tree, instances:Set[Instance]):Boolean =  {
      expr match {
        case app@Apply(fun, args) =>
          methodTraverser.traverse(app)
          val methodCallInfos = methodTraverser.methodCallInfos
          for (methodCallInfo <- methodCallInfos) {
            getClosestScopeAliasInfo(methodCallInfo(1), instances) match{
              case Some(aliasInfo) =>
                if(isProtocolMethod(methodCallInfo(0))) {
                  //reset the traverser's list to be empty
                  methodTraverser.methodCallInfos = ListBuffer[Array[String]]()
                  return true
                }
              case None =>
            }
          }
        case _ =>
      }
      //reset the traverser's list to be empty
      methodTraverser.methodCallInfos = ListBuffer[Array[String]]()
      false
    }

    def isProtocolMethod(methodName: String): Boolean = {  //change this to take methodname and type of element if there are multiple elements being checked
      if(currentElementInfo.methodToIndices.contains(methodName))
        return true
      false
    }

    def processMatchStatement(expr: Trees#Tree, cases: List[CaseDef], instances: Set[Instance]): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      if(!isAliasCallingProtocolMethod(expr, newInstances)) {
        val newInstancesAndReturned = checkInsideFunctionBody(expr, instances) //have this as two lines because just ._1 wasn't working here, oddly
        newInstances = newInstancesAndReturned._1
        newInstances = processCaseStatements(cases, newInstances)
      }
      else{
        newInstances = processCaseStatements(cases, newInstances, expr)
      }
      newInstances
    }

    def processCaseStatements(cases: List[global.CaseDef], instances: Set[Instance], expr:Trees#Tree=null): Set[Instance] = {
      //copy instances into newInstances properly like in if/else code
      var caseInstances: Set[Instance] = Set()
      for (instance <- instances) caseInstances += Instance(instance.className, instance.aliases, instance.currentStates)
      //this needs to actually process what is inside the case statement rather than the entire statement
      if(expr != null){
        var returnValue = cases.head.pat.toString()
        if(returnValue.contains("."))
          returnValue = returnValue.substring(returnValue.lastIndexOf('.')+1)
        caseInstances = updateStateIfNeeded(caseInstances, caseInstances, expr, ":"+returnValue)
      }
      val newInstances = checkInsideFunctionBody(cases.head.body, caseInstances)._1
      if (cases.tail.nonEmpty)
        Util.mergeInstanceStates(newInstances, processCaseStatements(cases.tail, instances, expr))
      else
        newInstances
    }

    def dealWithWhileLoop(cond: Trees#Tree, instances: Set[Instance], loopContent: Trees#Tree): Set[Instance] = {
      //initialisations
      var newInstances = for (instance <- instances) yield instance
      var instanceToInterimStates: mutable.HashMap[(String, Set[Alias]), ListBuffer[Set[State]]] = mutable.HashMap()
      for (instance <- newInstances) instanceToInterimStates += (instance.className, instance.aliases) -> ListBuffer(instance.currentStates)
      //loop
      do {
        //go through condition of the while
        if(cond != null)
          newInstances = checkInsideFunctionBody(cond, newInstances)._1
        Util.currentScope.push("while")
        //go through loop body
        newInstances = checkInsideFunctionBody(loopContent, newInstances)._1
        newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
        Util.currentScope.pop()
        for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases)))
          instanceToInterimStates((instance.className, instance.aliases)) += instance.currentStates
      } while (!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
      //go through condition of the while one last time before exiting as that is how the program will execute
      newInstances = checkInsideFunctionBody(cond, newInstances)._1
      //assigns interim states to the instances
      for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases)))
        for (setOfStates <- instanceToInterimStates((instance.className, instance.aliases)))
          instance.currentStates = instance.currentStates ++ setOfStates
      newInstances
    }

    def dealWithDoWhileLoop(cond: Trees#Tree, instances: Set[Instance], loopContent: Trees#Tree): Set[Instance] = {
      //initialisations
      var newInstances = for (instance <- instances) yield instance
      var instanceToInterimStates: mutable.HashMap[(String, Set[Alias]), ListBuffer[Set[State]]] = mutable.HashMap()
      for (instance <- newInstances) instanceToInterimStates += (instance.className, instance.aliases) -> ListBuffer()
      //loop
      do {
        Util.currentScope.push("dowhile")
        //go through loop body
        newInstances = checkInsideFunctionBody(loopContent, newInstances)._1
        newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
        Util.currentScope.pop()
        //go through condition of the while
        if(cond != null)
          newInstances = checkInsideFunctionBody(cond, newInstances)._1
        for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases)))
          instanceToInterimStates((instance.className, instance.aliases)) += instance.currentStates
      } while (!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
      //assigns interim states to the instances
      for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases))) {
        for (setOfStates <- instanceToInterimStates((instance.className, instance.aliases)))
          instance.currentStates = instance.currentStates ++ setOfStates
      }
      newInstances
    }

    def checkInsideForLoopGenerator(enums: Seq[Trees#Tree], instances: Set[Instance]): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      enums match {
        case List(fq"$pat <- $beginning until $end") =>
          for (begin <- beginning.children)
            newInstances = checkInsideFunctionBody(begin, newInstances)._1
          newInstances = checkInsideFunctionBody(end, newInstances)._1
        case List(fq"$pat <- $beginning to $end") =>
          for (begin <- beginning.children)
            newInstances = checkInsideFunctionBody(begin, newInstances)._1
          newInstances = checkInsideFunctionBody(end, newInstances)._1
        case List(fq"$pat <- $gen") =>
          newInstances = checkInsideFunctionBody(gen, newInstances)._1
        case _ => newInstances = checkInsideObjectBody(enums, newInstances)
      }
      newInstances
    }

    def dealWithForLoop(enums: Seq[Trees#Tree], instances: Set[Instance], loopContent: Trees#Tree): Set[Instance] = {
      //initialisations
      var newInstances = for (instance <- instances) yield instance
      var instanceToInterimStates: mutable.HashMap[(String, Set[Alias]), ListBuffer[Set[State]]] = mutable.HashMap()
      for (instance <- newInstances) instanceToInterimStates += (instance.className, instance.aliases) -> ListBuffer(instance.currentStates)
      //loop
      do {
        //go through condition of the for
        newInstances = checkInsideForLoopGenerator(enums, newInstances)
        Util.currentScope.push("for")
        //go through loop body
        newInstances = checkInsideFunctionBody(loopContent, newInstances)._1
        newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
        Util.currentScope.pop()
        //update map with new states of the instances
        println("new instances are " + newInstances)
        for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases)))
          instanceToInterimStates((instance.className, instance.aliases)) += instance.currentStates
        println("right before check for duplicates, map is " + instanceToInterimStates)
      } while (!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
      //assigns interim states to the instances
      for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases))) {
        for (setOfStates <- instanceToInterimStates((instance.className, instance.aliases)))
          instance.currentStates = instance.currentStates ++ setOfStates
      }
      newInstances
    }

    /** Handles try-catch in a basic manner, assuming no exceptions.
     * Just goes through try then finally bodies.
     *
     * @param tryBody     Code inside the try block.
     * @param cases       List of cases.
     * @param finallyBody Code inside the finally block.
     * @param instances   Instances passed in to update.
     * @return
     */
    def checkTryCatchFinally(tryBody: Trees#Tree, cases: List[CaseDef], finallyBody: Trees#Tree, instances: Set[Instance]): Set[Instance] = {
      var newInstances = checkInsideFunctionBody(tryBody, instances)._1
      newInstances = checkInsideFunctionBody(finallyBody, newInstances)._1
      newInstances
    }

    /** Handles an if-else statement. Saves the current state of instances in ifInstances and elseInstances then
     * goes through both paths and gets new states for the instances. Once this is done it merges the two possible
     * paths and instances now hold all possible states they could be in after going through either path.
     *
     * @param ifBody    Code inside the if block
     * @param elseBody  Code inside the else block
     * @param instances Instances to update
     * @return
     */
    def dealWithIfElse(condition: Trees#Tree, ifBody: Trees#Tree, elseBody: Trees#Tree, instances: Set[Instance]): (Set[Instance], Option[Set[Instance]]) = {
      var newInstances = for (instance <- instances) yield instance
      newInstances = checkInsideFunctionBody(condition, newInstances)._1
      var ifInstances: Set[Instance] = Set()
      for (instance <- newInstances) ifInstances += Instance(instance.className, instance.aliases, instance.currentStates)
      var elseInstances: Set[Instance] = Set()
      for (instance <- newInstances) elseInstances += Instance(instance.className, instance.aliases, instance.currentStates)
      val (newIfInstances, returnedIfOption) = checkInsideFunctionBody(ifBody, ifInstances)
      var returnedIf: Set[Instance] = Set()
      returnedIfOption match {
        case Some(returnedIfValue) =>
          returnedIf = returnedIfValue
        case None =>
      }
      val (newElseInstances, returnedElseOption) = checkInsideFunctionBody(elseBody, elseInstances)
      var returnedElse: Set[Instance] = Set()
      returnedElseOption match {
        case Some(returnedElseValue) =>
          returnedElse = returnedElseValue
        case None =>
      }
      val returnedIfElse = Option(returnedIf ++ returnedElse)
      (mergeInstanceStates(newIfInstances, newElseInstances), returnedIfElse)
    }

    /** Handles code which creates a new instance of a class if it is the protocolled class we are checking.
     * Checks if the new instance is of the class we are currently handling and then checks if the instance
     * is already defined in the list of instances.
     * If so, it replaces the old instance with a new one. If not it adds a new instance to the list.
     *
     * Boolean used so y = if()x else new Class does not delete y from the instance with x
     *
     * @param name      Name of the new instance
     * @param instances Instances to update
     * @return
     */
    def processNewInstance(name: String, instances: Set[Instance], dontRemove: Boolean = false): Set[Instance] = {
      val className = currentElementInfo.name
      val states = currentElementInfo.states
      var newInstances = for (instance <- instances) yield instance
      if (!dontRemove) newInstances = Util.removeAliasesInScope(newInstances, name, Util.currentScope)
      newInstances += Instance(className, Set(Alias(name, Util.currentScope.clone)), Set(states(0)))
      newInstances
    }


    /** Gets the instance with the closest scope to the current scope with the given name if it exists. If not,
     * returns None.
     * Works by copying the current scope and then checking if there is an instance which matches name and the copied
     * scope. If not then it will reduce the current scope and do the same search there until it finds the instance
     * with the same name with the longest possible scope which is still a subset of the current scope.
     *
     * @param name
     * @param instances
     * @return
     */
    def getClosestScopeAliasInfo(name: String, instances: Set[Instance]): Option[(String, mutable.Stack[String])] = {
      if (instances.isEmpty) return None
      val curScope = Util.currentScope.clone()
      while (curScope.nonEmpty) {
        for (instance <- instances) {
          for (alias <- instance.aliases if alias.name == name && alias.scope == curScope) {
            return Some(alias.name, alias.scope)
          }
        }
        curScope.pop()
      }
      None
    }

    /** Gets the named object of closest scope to the current scope.
     *
     * @param objectName
     * @return
     */
    def getClosestScopeObject(objectName: String): Option[ClassOrObject] = {
      val classesAndObjects = classAndObjectTraverser.classesAndObjects
      if (classesAndObjects.isEmpty) return None
      val curScope = Util.currentScope.clone()
      while (curScope.nonEmpty) {
        for (element <- classesAndObjects) {
          if (element.name == objectName && element.scope == curScope) {
            return Option(element)
          }
        }
        curScope.pop()
      }
      None
    }

    /** Checks if the object (calledOn) is defined in the file and if it has not already been initialised, runs the
     * code inside the object.
     *
     * @param calledOn  The object that the method is called on and the one we want to check the constructor for if
     *                  that hasn't been done already
     * @param instances Instances to update
     */
    def checkObjectFunctionCall(calledOn: global.Tree, instances: Set[Instance]): Unit = {
      if (calledOn == null) return
      var calledOnString = calledOn.toString()
      if (calledOnString.lastIndexOf(".") != -1)
        calledOnString = calledOn.toString.substring(calledOn.toString.lastIndexOf(".") + 1)
      for (element <- classAndObjectTraverser.classesAndObjects
           if (!element.initialised && element.isObject && element.name == calledOnString
             && element.scope == getScope(calledOn))) {
        element.initialised = true
        Util.currentScope.push(calledOnString)
        checkInsideObjectBody(element.body, instances)
        Util.currentScope.pop()
      }
    }

    def renameAliasesBack(paramNameScopeToAlias: mutable.HashMap[(String, mutable.Stack[String]), (String, mutable.Stack[String])], instances: Set[Instance]): Set[Instance] = {
      for ((parameter, alias) <- paramNameScopeToAlias) {
        for (instance <- instances if instance.aliases.contains(Alias(parameter._1, parameter._2))) {
          instance.aliases -= Alias(parameter._1, parameter._2)
          instance.aliases += Alias(alias._1, alias._2)
        }
      }
      instances
    }

    def hasMoreValuesThanKeys(givenToFunctionParams: mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]): Boolean = {
      for ((givenParam, functionParams) <- givenToFunctionParams) {
        if (functionParams.size > 1) return true
      }
      false
    }

    def isAssignmentFunction(funcCall: global.Apply, instances: Set[Instance]): (Boolean, Set[Instance]) = {
      funcCall match {
        case Apply(Select(arg1, TermName(functionName)), List(arg3)) =>
          val regex = ".*_\\$eq".r
          regex.findPrefixMatchOf(functionName) match {
            case Some(mat) =>
              val value = mat.toString
              val newInstances = processAssignment(value.substring(0, value.length - 4), arg3, instances)
              return (true, newInstances)
            case None =>
          }
        case _ =>
      }
      (false, instances)
    }

    def assignParameters(instances: Set[Instance], parameters: ArrayBuffer[Array[String]], args: List[Tree], calledOn: Tree): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      if (calledOn != null && calledOn.symbol.tpe.toString().contains(currentElementInfo.name)) {
        newInstances = processNovelAssignment(currentElementInfo.name, calledOn, newInstances)
      }
      var i = 0
      for (param <- parameters) {
        if (param.length > 1) {
          if (param(1).contains(currentElementInfo.name)) {
            newInstances = processNovelAssignment(param(0), args(i), newInstances)
          }
        }
        i += 1
      }
      println("after assigning parameters, instances are " + newInstances)
      newInstances
    }

    def checkArguments(args: List[global.Tree], instances: Set[Instance]): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      var i = 0
      for (arg <- args) {
        val instancesAndReturned = checkInsideFunctionBody(arg, instances)
        newInstances = instancesAndReturned._1
        i += 1
      }
      newInstances
    }

    def cacheContainsCurrentStates(map: Map[ArrayBuffer[(String, Set[String], Set[State])], ArrayBuffer[Set[State]]],
                                   array2: ArrayBuffer[(String, Set[String], Set[State])]):
    (Boolean, ArrayBuffer[(String, Set[String], Set[State])]) = {
      for ((array, states) <- map) {
        val set1 = array.toSet
        val set2 = array2.toSet
        if (set1.equals(set2)) return (true, array)
      }
      (false, null)
    }

    def createCacheEntry(givenToFunctionParams: mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]],
                         instances: Set[Instance]): ArrayBuffer[(String, Set[String], Set[State])] = {
      val cacheEntry = ArrayBuffer[(String, Set[String], Set[State])]()
      for ((aliasInfo, paramInfos) <- givenToFunctionParams) {
        var currentStates = Set[State]()
        for (paramInfo <- paramInfos) {
          val instancesToGetStatesFrom = instances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
          for (instance <- instancesToGetStatesFrom) {
            currentStates ++= instance.currentStates
          }
        }
        var paramNames = Set[String]()
        for (paramInfo <- paramInfos) paramNames += paramInfo._1
        cacheEntry.append((currentElementInfo.name, paramNames, currentStates))
      }
      cacheEntry
    }

    def mutateInstances(parameters: ArrayBuffer[(String, Set[String], Set[State])], instances: Set[Instance], function: Function): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      var i = 0
      for ((element, parameterNames, states) <- parameters) {
        for (paramName <- parameterNames) {
          getClosestScopeAliasInfo(paramName, newInstances) match {
            case Some(paramInfo) =>
              val instancesToMutate = newInstances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
              for (instance <- instancesToMutate) instance.currentStates = function.stateCache(parameters)(i)
            case None =>
          }
        }
        i += 1
      }
      newInstances
    }

    def findNextStates(cacheEntry: ArrayBuffer[(String, Set[String], Set[State])], newInstances: Set[Instance]): ArrayBuffer[Set[State]] = {
      val nextStatesArray = ArrayBuffer[Set[State]]()
      for ((element, parameterNames, states) <- cacheEntry) {
        var nextStates = Set[State]()
        for (paramName <- parameterNames) {
          getClosestScopeAliasInfo(paramName, newInstances) match {
            case Some(paramInfo) =>
              val instancesWithNextStates = newInstances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
              for (instance <- instancesWithNextStates) nextStates ++= instance.currentStates
            case None =>
          }
        }
        nextStatesArray.append(nextStates)
      }
      nextStatesArray
    }

    def typesMatch(firstParameters:ArrayBuffer[Array[String]], secondParameters:List[global.Tree]): Boolean ={
      if(!firstParameters.exists(param => param(0).length > 0) && secondParameters.isEmpty) return true
      for((param, i) <- firstParameters.zipWithIndex) {
        if(param(1) != Util.keepOnlyName(secondParameters(i).tpe.toString()))
          return false
      }
      true
    }

    /** Checks function calls.
     * First it checks if the function is an assignment in which case it just returns to let the assignment
     * be dealt with in the assignment function
     * Then it checks if the function is new x and therefore the code inside a class should be analysed.
     * Then it checks if an object is being called on for the first time and its code should be analysed.
     * Then it goes to analyse inside the function body, renaming the instances to parameter names if needed.
     *
     * @param funcCall
     * @param functionName
     * @param args
     * @param instances
     * @param calledOn
     * @return
     */
    def dealWithFunction(funcCall: global.Apply, functionName: global.Name, args: List[global.Tree], instances: Set[Instance], calledOn: Tree = null):
    (Set[Instance], Option[Set[Instance]]) = {
      //region <Checks>

      //check for an assignment function, don't want to check args or do anything else in this case
      val isAssignmentAndInstances = isAssignmentFunction(funcCall, instances)
      if (isAssignmentAndInstances._1) return (isAssignmentAndInstances._2, None)

      //checks and gets parameters
      var newInstances = checkArguments(args, instances)

      //checks for "new Class()" constructor function
      val isCurrentIsNewInstancesAndReturned = checkNewFunction(funcCall, args, instances)
      if (isCurrentIsNewInstancesAndReturned._2) {
        if (isCurrentIsNewInstancesAndReturned._1)
          return (isCurrentIsNewInstancesAndReturned._3, Some(isCurrentIsNewInstancesAndReturned._4))
        else return (isCurrentIsNewInstancesAndReturned._3, None)
      }
      //checks for Object constructor call
      checkObjectFunctionCall(calledOn, instances)
      //endregion
      //finding function definition
      for (function <- functionTraverser.functions
           if (function.name == functionName.toString()
             && function.scope == getScope(funcCall, dontCheckSymbolField = true)
             && typesMatch(function.params, args))) {
        println("found function " + function.name)
        Util.currentScope.push(function.name) //push scope so parameters will be scoped inside the function
        newInstances = assignParameters(newInstances, function.params, args, calledOn)
        //create maps
        val mapsAndInstances = createMaps(function.params, function.name, newInstances)
        val functionToGivenParams = mapsAndInstances._1 //REMOVE THIS??
        val givenToFunctionParams = mapsAndInstances._2
        //make possible cache entry
        val cacheEntry = createCacheEntry(givenToFunctionParams, newInstances)
        //check if it hits
        val cacheHitAndParams = cacheContainsCurrentStates(function.stateCache, cacheEntry)
        val cacheHit = cacheHitAndParams._1
        val parameters = cacheHitAndParams._2
        //mutate state if possible and skip recursive call if needed
        if (cacheHit && function.stateCache(parameters) != null) {
          mutateInstances(parameters, newInstances, function)
          newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
          println("skipping")
          Util.currentScope.pop()
          return (newInstances, function.returned)
        }
        if (cacheHit && function.stateCache(parameters) == null) {
          newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
          println("recursing")
          Util.currentScope.pop()
          return (newInstances, function.returned)
        }
        //if it doesn't, put in a null entry
        function.stateCache += cacheEntry -> null
        //check inside the function body
        Util.currentScope.push("body")
        val newInstancesAndReturned = checkInsideFunctionBody(function.body, newInstances)
        newInstances = newInstancesAndReturned._1
        //figuring out what is returned PUT THIS INTO ITS OWN FUNCTION
        newInstancesAndReturned._2 match {
          case Some(setOfInstances) =>
            var scopeClone = Util.currentScope.clone()
            var instancesReturned = setOfInstances ++ Set(Instance(null, Set(Alias("scope", scopeClone.clone())), Set())) //delete internal variables
            scopeClone.pop()
            instancesReturned = instancesReturned ++ Set(Instance(null, Set(Alias("scope", scopeClone)), Set())) //need to delete the parameters too
            function.returned = Some(Util.copyInstances(instancesReturned))
          case None =>
            function.returned = Some(Set(Instance(null, Set(Alias("scope", Util.currentScope.clone())), Set())))
        }
        //remove aliases inside the body of the function since they can't be used anymore
        newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
        Util.currentScope.pop()
        //construct array of next states
        val nextStates = findNextStates(cacheEntry, newInstances)
        //update cache
        function.stateCache += cacheEntry -> nextStates
        //delete aliases in function here
        newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
        println(s"the deal with function returns ${function.returned}")
        Util.currentScope.pop()
        val returned = function.returned
        return (newInstances, returned)
      }
      (instances, None)
    }

    /** Checks if the object given has been seen before. If not, executes the code inside it.
     *
     * @param objectName
     * @param instances
     * @return
     */
    def checkObject(objectName: String, instances: Set[Instance] = Set()): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      getClosestScopeObject(objectName) match {
        case Some(obj) =>
          obj.initialised = true
          Util.currentScope.push(objectName)
          newInstances = checkInsideObjectBody(obj.body, instances)
          Util.currentScope.pop()
        case _ =>
      }
      newInstances
    }


    /** Checks for a new x function and executes the code within the class if found. Renames instances to
     * constructor parameter names if needed.
     *
     * @param funcCall
     * @param instances
     */
    def checkNewFunction(funcCall: global.Apply, args: List[Tree], instances: Set[Instance]): (Boolean, Boolean, Set[Instance], Set[Instance]) = {
      var newInstances = for (instance <- instances) yield instance
      var isCurrentType = false
      var returned: Set[Instance] = Set()
      funcCall match {
        case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          parents match {
            case List(Ident(name)) =>
              //note here getScope is called on the symbol owner of funcCall since we want to skip the class name in the scope
              val newInstancesAndIsCurrentTypeAndReturned = checkInsideClass(getScope(funcCall.symbol.owner), name.toString(), args, newInstances)
              newInstances = newInstancesAndIsCurrentTypeAndReturned._1
              isCurrentType = newInstancesAndIsCurrentTypeAndReturned._2
              returned = newInstancesAndIsCurrentTypeAndReturned._3
            case List(Apply(className, arg2)) =>
              val newInstancesAndIsCurrentTypeAndReturned = checkInsideClass(getScope(className), className.toString(), args, newInstances)
              newInstances = newInstancesAndIsCurrentTypeAndReturned._1
              isCurrentType = newInstancesAndIsCurrentTypeAndReturned._2
              returned = newInstancesAndIsCurrentTypeAndReturned._3
            case _ =>
          }
          return (isCurrentType, true, newInstances, returned)
        case _ =>
      }
      (false, false, newInstances, returned)
    }

    def checkInsideClass(scope: mutable.Stack[String], elementNameString: String, args: List[Tree], instances: Set[Instance]):
    (Set[Instance], Boolean, Set[Instance]) = {
      var newInstances = for (instance <- instances) yield instance
      var returned: Set[Instance] = null
      var elementName = elementNameString
      if (elementName.lastIndexOf(".") != -1)
        elementName = elementName.substring(elementName.lastIndexOf(".") + 1)
      for (element <- classAndObjectTraverser.classesAndObjects
           if (!element.isObject && element.name == elementName && element.scope == scope)) {
        assignParameters(newInstances, element.params, args, null)
        Util.currentScope.push(element.name)
        newInstances +=
          Instance(currentElementInfo.name, Set(Alias(currentElementInfo.name, Util.currentScope.clone())), Set(currentElementInfo.states(0)))
        newInstances = checkInsideObjectBody(element.body, newInstances)
        val scopeInstance = Instance(null, Set(Alias("scope", Util.currentScope.clone())), Set())
        for (instance <- newInstances) {
          if (instance.containsAliasInfo(currentElementInfo.name, Util.currentScope))
            returned = Set(Instance(instance.className, instance.aliases, instance.currentStates), scopeInstance)
        }
        newInstances = Util.removeAllAliasesInScope(newInstances, Util.currentScope)
        Util.currentScope.pop()
      }
      (newInstances, elementName == currentElementInfo.name, returned)
    }

    /** For the parameter list given, checks if they match any of our defined instances. If so, renames the instance
     * to the parameter name. Keeps memory of the renaming in a hashmap of parameter name to instance name so these
     * can easily be renamed after the function exits.
     *
     * @param instances
     * @return
     */
    def createMaps(parameters: ArrayBuffer[Array[String]], functionName: String, instances: Set[Instance]):
    (mutable.HashMap[(String, mutable.Stack[String]), (String, mutable.Stack[String])],
      mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]) = {
      var functionToGivenParams = new mutable.HashMap[(String, mutable.Stack[String]), (String, mutable.Stack[String])]
      var givenToFunctionParam = new mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]
      //construct maps
      var argCounter = 0
      val paramScope = Util.currentScope.clone()
      for (arg <- parameters) {
        val argString = arg(0)
        getClosestScopeAliasInfo(argString, instances) match {
          case Some(aliasInfo) =>
            val paramName = parameters(argCounter)(0)
            functionToGivenParams += (paramName, paramScope) -> (aliasInfo._1, aliasInfo._2)
            givenToFunctionParam.get(aliasInfo._1, aliasInfo._2) match {
              case Some(setOfParams) =>
                val updatedSet = setOfParams ++ Set((paramName, paramScope))
                givenToFunctionParam += (aliasInfo._1, aliasInfo._2) -> updatedSet
              case None =>
                givenToFunctionParam += (aliasInfo._1, aliasInfo._2) -> Set((paramName, paramScope))
            }
          case None =>
        }
        argCounter += 1
      }
      (functionToGivenParams, givenToFunctionParam)
    }

    /** Handles any for or while loop.
     * It goes through the contents of the for loop and checks what the states of all the instances are at the end.
     * It stores theses states in a list (one for each instance) and checks to see if all instances have looped
     * (i.e. they have twice the same set of states in their list). If so it gets out of the for loop. It then
     * gives all instances all the states they went through while looping since we don't know how many times the loop
     * will iterate between 0 and infinity times. This assumes that users cannot write loops of infinite length into their
     * protocols, otherwise this would never terminate.
     *
     * @param instances
     * @param loopContent
     * @return
     */
    def dealWithLoopContents(instances: Set[Instance], loopContent: Trees#Tree): Set[Instance] = {
      var newInstances = for (instance <- instances) yield instance
      var instanceToInterimStates: mutable.HashMap[(String, Set[Alias]), ListBuffer[Set[State]]] = mutable.HashMap()
      for (instance <- newInstances) instanceToInterimStates += (instance.className, instance.aliases) -> ListBuffer(instance.currentStates)
      do {
        for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases)))
          instanceToInterimStates((instance.className, instance.aliases)) += instance.currentStates
        for (line <- loopContent) {
          newInstances = processLine(line, newInstances)._1
          for (updatedInstance <- newInstances if instanceToInterimStates.contains((updatedInstance.className, updatedInstance.aliases))) {
            instanceToInterimStates((updatedInstance.className, updatedInstance.aliases))(instanceToInterimStates((updatedInstance.className, updatedInstance.aliases)).length - 1) = updatedInstance.currentStates
          }
        }
      } while (!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
      for (instance <- newInstances if instanceToInterimStates.contains((instance.className, instance.aliases))) {
        for (setOfStates <- instanceToInterimStates((instance.className, instance.aliases)))
          instance.currentStates = instance.currentStates ++ setOfStates
      }
      newInstances
    }

    /** For a given line of code, checks if it is a method on an instance with protocol and if so updates its state
     *
     * @param instancesAfterFunction
     * @param line
     */
    def updateStateIfNeeded(instancesBeforeFunction: Set[Instance], instancesAfterFunction: Set[Instance], line: Trees#Tree, returnValue:String=""): Set[Instance] = {
      println("CALLED UPDATE")
      line match {
        case app@Apply(fun, args) =>
          methodTraverser.traverse(app)
        case _ =>
      }
      val methodCallInfos = methodTraverser.methodCallInfos
      for (methodCallInfo <- methodCallInfos) {
        var methodName = methodCallInfo(0)+returnValue
        if(methodName.contains(".") && methodName.contains("("))
          methodName = methodName.substring(0,methodName.indexOf("(")+1) + methodName.substring(methodName.lastIndexOf(".")+1)
        val aliasName = methodCallInfo(1)
        println("method name is " + methodName)
        println("instances inside update are " + instancesAfterFunction)
        breakable {
          getClosestScopeAliasInfo(aliasName, instancesAfterFunction) match {
            case Some(aliasInfo) =>
              if (!currentElementInfo.methodToIndices.contains(methodName) && !currentElementInfo.returnValueToIndice.contains(methodName)) {
                break
              }
              val instancesToUpdate = instancesAfterFunction.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              val originalInstances = instancesBeforeFunction.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              if (!(instancesToUpdate == originalInstances)) {
                for (instance <- originalInstances) {
                  updateInstance(instance, methodName, line)
                }
                if (!(instancesToUpdate == originalInstances)) {
                  var expectedStates:Set[State] = Set()
                  for(instance <- originalInstances){
                    expectedStates ++= instance.currentStates
                  }
                  var actualStates:Set[State] = Set()
                  for(instance <- instancesToUpdate){
                    actualStates ++= instance.currentStates
                  }
                  throw new inconsistentStateMutation(methodName, aliasInfo._1,
                    line.pos.source.toString(), line.pos.line, expectedStates, actualStates)
                }
              }
              else {
                for (instance <- instancesToUpdate) {
                  updateInstance(instance, methodName, line)
                }
              }
            case None =>
          }
        }
      }
      //reset the traverser's list to be empty
      methodTraverser.methodCallInfos = ListBuffer[Array[String]]()
      println("instances at the end of update if needed are " + instancesAfterFunction)
      instancesAfterFunction
    }

    def updateInstance(instance: Instance, methodName: String, line: Trees#Tree) = {
      var newSetOfStates: Set[State] = Set()
      for (state <- instance.currentStates) {
        println("found method name " + methodName)
        var indexSet:Set[Int] = Set()
        if(currentElementInfo.methodToIndices.contains(methodName))
          indexSet = currentElementInfo.methodToIndices(methodName)
        else
          indexSet = Set(currentElementInfo.returnValueToIndice(methodName))
        var newStates: Set[State] = Set[State]()
        newStates += currentElementInfo.transitions(state.index)(indexSet.min)
        if (indexSet.size > 1 && currentElementInfo.transitions(state.index)(indexSet.min).name == Util.Undefined)
          newStates = for (x <- indexSet - indexSet.min) yield currentElementInfo.transitions(state.index)(x)
        println("new states are " + newStates)
        for (state <- newStates if state.name == Util.Undefined) {
          throw new protocolViolatedException(Util.sortSet(instance.getAliasNames()), currentElementInfo.name,
            Util.sortSet(instance.currentStates), methodName, line.pos.source.toString(), line.pos.line)
        }
        newSetOfStates = newSetOfStates ++ newStates
      }
      instance.currentStates = newSetOfStates
    }

    //region<Traversers>
    /** Traverses a tree and collects (methodName, aliasName) from method application statements
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
                  case select@Select(qualifier, name) =>
                    var instanceName = qualifier.toString()
                    if (qualifier.hasSymbolField) instanceName = qualifier.symbol.name.toString
                    methodCallInfos +=
                      Array(/*_*/ name.toString().appendedAll(getParametersFromTree(exprss)) /*_*/ ,
                        instanceName)
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


    /** Traverses a tree and collects classes and objects found */
    object classAndObjectTraverser extends Traverser {
      var classesAndObjects = ListBuffer[ClassOrObject]()

      override def traverse(tree: Tree): Unit = {
        tree match {
          case obj@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
            classesAndObjects += ClassOrObject(tname.toString, ArrayBuffer(), body, getScope(obj), isObject = true)
            super.traverse(obj)
          case cla@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
            val parameters = /*_*/ getParametersWithInstanceNames(paramss) /*_*/
            classesAndObjects += ClassOrObject(tpname.toString(), parameters, stats, getScope(cla))
            super.traverse(cla)
          case _ =>
            super.traverse(tree)
        }
      }
    }

    /** Gathers function definitions in a tree inside "functions" */
    object functionTraverser extends Traverser {
      var functions = ListBuffer[Function]()

      override def traverse(tree: Tree): Unit = {
        tree match {
          case func@q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
            val parameters = /*_*/ getParametersWithInstanceNames(paramss) /*_*/
            if (tname.toString() != "<init>")
              functions +=
                Function(tname.toString(), parameters, tpt, expr,
                  getScope(func), Map[ArrayBuffer[(String, Set[String], Set[State])], ArrayBuffer[Set[State]]](), None)
            /*_*/ super.traverse(expr) /*_*/
          case _ =>
            super.traverse(tree)
        }
      }
    }


//endregion
    /** Removes alias from instances */
    def removeAliases(instances: Set[Instance], aliasName: String): Set[Instance] = {
      val newInstances = for (instance <- instances) yield instance
      getClosestScopeAliasInfo(aliasName, newInstances) match {
        case Some(aliasInfo) =>
          val instancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
          for (instance <- instancesToUpdate)
            instance.aliases -= Alias(aliasInfo._1, aliasInfo._2)
        case None =>
      }
      cleanInstances(newInstances)
    }

    /** Gets the scope of a tree from its symbol.
     * It will check if the tree has a symbol field unless specified not to.
     * The boolean value is there because some trees will not pass the hasSymbolField check even though it is
     * possible to get their scope in this way. I don't know why.
     *
     * @param obj
     * @param dontCheckSymbolField
     * @return
     */
    def getScope(obj: Tree, dontCheckSymbolField: Boolean = false): mutable.Stack[String] = {
      val objectScope = mutable.Stack[String]()
      if (obj.hasSymbolField || dontCheckSymbolField) {
        for (symbol <- obj.symbol.owner.ownerChain.reverse)
          objectScope.push(symbol.name.toString())
      }
      objectScope
    }

    def getScope(symbol: Symbol): mutable.Stack[String] = {
      val objectScope = mutable.Stack[String]()
      for (symbol <- symbol.owner.ownerChain.reverse)
        objectScope.push(symbol.name.toString())
      objectScope
    }

    /** Gets a parameter string as formatted in a function definition from a tree of them */
    def getParametersFromTree(params: List[List[Tree]]): String = {
      params match {
        case List(List()) => "()"
        case List(List(value)) => Util.keepOnlyName(value.tpe.toString()).mkString("(", "", ")")
        case List(values) =>
          var parameters: ArrayBuffer[String] = ArrayBuffer()
          for (elem <- values) {
            parameters += Util.keepOnlyName(elem.tpe.toString)
          }
          parameters.mkString("(", ",", ")")
        case _ => ""
      }
    }

    /** Checks that methods in return values are a subset of those in stats
     *
     * @param returnValuesArray
     * @param stats
     * @param elementName
     * @param filename
     */
    def checkProtocolMethodsSubsetClassMethods(returnValuesArray: Array[ReturnValue], stats: Seq[Trees#Tree], elementName: String, filename: String): Unit = {
      val classMethods = getMethodNames(stats)
      var protocolMethods: Set[String] = Set()
      for (i <- returnValuesArray.indices) {
        protocolMethods += Util.stripReturnValue(returnValuesArray(i).parentMethod.name.replaceAll("\\s", ""))
      }
      if (!(protocolMethods subsetOf classMethods)) throw new badlyDefinedProtocolException(
        s"Methods $protocolMethods defined in $filename are not a subset of methods " +
          s"$classMethods defined in class $elementName. Methods ${protocolMethods.diff(classMethods)} are defined in " +
          s"the protocol but not in the class")
    }


    /** Checks if an annotation is a Typestate annotation and returns the filename if so
     *
     * @param annotation
     * @return
     */
    def getFilenameFromTypestateAnnotation(annotation: AnnotationInfo): Option[String] = {
      annotation match {
        case AnnotationInfo(arg1, arg2, arg3) =>
          if (arg1.toString == "Typestate" || arg1.toString == "compilerPlugin.Typestate") {
            Some(arg2.head.toString())
          }
          else None
        case _ => None
      }
    }

    /** Returns a set of method names (as name(parameters)) from the body of a class
     *
     * @param methodBody
     * @return
     */
    def getMethodNames(methodBody: Seq[Trees#Tree]): Set[String] = {
      var methodNames: Set[String] = Set()
      for (line <- methodBody) {
        line match {
          case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
            val parameters = /*_*/ getParameters(paramss) /*_*/
            methodNames += tname + s"($parameters)"
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
    def getParameters(params: List[List[ValDef]]): String = {
      params match {
        case List(List()) => ""
        case List(List(value)) =>
          var valueName = value.tpt.toString()
          if(valueName.contains('.'))
            valueName = valueName.substring(valueName.lastIndexOf('.')+1)
          valueName
        case List(values) =>
          var parameters: ArrayBuffer[String] = ArrayBuffer()
          for (elem <- values) {
            var valueName = elem.tpt.toString
            if(valueName.contains('.'))
              valueName = valueName.substring(valueName.lastIndexOf('.')+1)
            parameters += valueName
          }
          parameters.mkString(",")
        case _ => ""
      }
    }


    /** Gets parameters from a tree as their name and type in a string array */
    def getParametersWithInstanceNames(params: List[List[ValDef]]): ArrayBuffer[Array[String]] = {
      params match {
        case List(List()) => ArrayBuffer(Array(""))
        case List(List(value)) => ArrayBuffer(Array(value.name.toString(), value.tpt.toString()))
        case List(values) =>
          var parameters: ArrayBuffer[Array[String]] = ArrayBuffer()
          for (elem <- values) {
            parameters += Array(elem.name.toString(), elem.tpt.toString)
          }
          parameters
        case _ => ArrayBuffer()
      }
    }
  }

}







