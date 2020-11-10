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
import com.sun.jmx.snmp.Enumerated


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

  case class Function(name: String, params: ArrayBuffer[(String, Type)],
                      returnType: Trees#Tree, body: Trees#Tree, scope: mutable.Stack[String],
                      var stateCache: Map[ArrayBuffer[(Type, Set[String], Set[State])], ArrayBuffer[Set[State]]],
                      var returned: Option[Set[Instance]]) { //might replace string with elementInfo for more precision (on build)
    override def toString: String = {
      s"name: $name parameters: $params return type: $returnType scope: $scope body: $body stateCache: $stateCache returned: $returned"
    }
  }
  case class ClassOrObject(elementType:Type, params:ArrayBuffer[(String, Type)], body:Seq[Trees#Tree], scope:mutable.Stack[String],
                           isObject:Boolean=false, var initialised:Boolean=false){
    override def toString(): String={ s"$elementType ${showParams(params)} $scope $initialised" }

    def showParams(params:ArrayBuffer[(String, Type)]):String={
      var parameters = ""
      for(param <- params) {
        parameters += param._1+": "+param._2
        parameters += " ; "
      }
      parameters
    }
  }

  val runsAfter: List[String] = List[String]("refchecks")
  val phaseName: String = "compilerPlugin.GetFileFromAnnotation.this.name"

  def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

  /** Phase which is ran by the plugin */
  class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
    var compilationUnit: CompilationUnit = _
    var currentElementInfo: ElementInfo = _
    var savedBreakInstances: mutable.Map[Type, mutable.Map[String, ArrayBuffer[Set[Instance]]]] =
      mutable.Map[Type, mutable.Map[String, ArrayBuffer[Set[Instance]]]]()
    var protocolledElements: mutable.Map[Type, ElementInfo] = mutable.Map[Type, ElementInfo]()

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
      for (tree@q"$mods class $className[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
        checkElement(body, className.tpe, getScope(tree), tree)
      }
      for (tree@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
        val objectName = mutable.Stack[String]()
        objectName.push(name.toString())
        checkElement(body, name.tpe, objectName, tree, isObject = true)
      }
    }

    /** Checks whether the object or class is following its protocol in the code.
     * It first checks if the element has a typestate annotation, then runs the protocol and collects the information
     * from it.
     * Then it checks the methods in the protocol are a subset of those defined in the element.
     * Then it checks the protocol is followed
     *
     * @param body     The code to check
     * @param elementType     Name of the element
     * @param tree     The entire code
     * @param isObject Whether or not the element to check is an Object (as opposed to a Class)
     * @return
     */
    def checkElement(body: Seq[Trees#Tree], elementType: Type, scope: mutable.Stack[String], tree: Tree, isObject: Boolean = false)= {
      val annotations = tree.symbol.annotations
      for (annotation@AnnotationInfo(arg1, arg2, arg3) <- annotations) {
        getFilenameFromTypestateAnnotation(annotation) match {
          case Some(protocolName) => //a correct Typestate annotation is being used
            println("protocol name is "+protocolName)
            //retrieve the serialized data
            if (!Files.exists(Paths.get(s"compiledProtocols\\$protocolName.ser")))
              throw new badlyDefinedProtocolException(s"The protocol $protocolName could not be processed, " +
                s"check that the protocol name is the same as the name of the object containing your protocol")
            val (stateMachine, states, returnValuesArray) = Util.getDataFromFile(s"compiledProtocols\\$protocolName.ser")
            checkProtocolMethodsSubsetClassMethods(returnValuesArray, body, elementType, protocolName)
            val methodToIndices = Util.createMethodToIndicesMap(returnValuesArray)
            val returnValueToIndice = Util.createReturnValueToIndiceMap(returnValuesArray)
            val stateToAvailableMethods = Util.createStateToAvailableMethodsMap(returnValuesArray)
            println("state to available methods: "+stateToAvailableMethods)
            currentElementInfo = ElementInfo(stateMachine, states, methodToIndices, returnValueToIndice, Set[Instance]())
            if(isObject)
              currentElementInfo.instances += Instance(Set(Alias(elementType.toString(), Util.currentScope.clone)), Set(states(0)))
            protocolledElements += elementType -> currentElementInfo
            println("map with protocolled elements is "+protocolledElements)
            println("after being set, cei is "+currentElementInfo)
          case None =>
        }
      }
      checkElementIsUsedCorrectly()
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
                  checkInsideFunctionBody(expr)
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
    def checkInsideObjectBody(code: Seq[Trees#Tree]) = {
      for(line <- code) {
        checkInsideFunctionBody(line)
      }
      println("\nInstances:")
      protocolledElements.foreach((element) => element._2.instances.foreach(println))
    }


    /** Checks the code inside the body of a function for protocol violations.
     * Goes line by line and skips lines if needed (for example when at a definition or a loop).
     *
     * @param code           The code to check
     * @param givenInstances Set of instances given to be updated (optional). Will create a new set if not
     * @return
     */
    def checkInsideFunctionBody(code: Trees#Tree): (Option[Set[Instance]]) = {
      var returned: Option[Set[Instance]] = None
      var nbOfLinesToSkip = 0
      for (line <- code) {
        breakable {
          if (nbOfLinesToSkip > 0) {
            nbOfLinesToSkip -= 1
            break
          }
          val NbLinesToSkipAndReturned = processLine(line)
          nbOfLinesToSkip = NbLinesToSkipAndReturned._1
          if(nbOfLinesToSkip == -1)
              return None
          if (NbLinesToSkipAndReturned._2.isDefined) // do this to avoid erasing returned element
            returned = NbLinesToSkipAndReturned._2
        }
      }
      println("\nInstances:")
      protocolledElements.foreach((element) => element._2.instances.foreach(println))
      returned
    }



    /** Checks a line and returns possibly updated instances.
     * Has different cases for different types of line
     *
     * @param line      line of code to analyse
     * @param instances instances to update
     * @return
     */
    def processLine(line: Trees#Tree): (Int, Option[Set[Instance]]) = {
      println(s"checking line at line number " + line.pos.line)
      println(s"instances before checking line are "+protocolledElements.foreach((element) => element._2.instances.foreach(println)))
      //println(showRaw(line))
      line match {
        case Apply(Select(Select(scope, label), TermName("break")), body) =>
          dealWithBreak(label.toString())
          (-1, None)
        case Apply(Select(Ident(TermName(label)), TermName("break")), List()) =>
          dealWithBreak(label)
          (-1, None)
        case Apply(Select(Select(scope, label), TermName("breakable")), body) =>
          dealWithBreakable(label, body)
          (Util.getLengthOfTree(line) - 1, None)
        case Apply(Select(Ident(label), TermName("breakable")), body) =>
          dealWithBreakable(label, body)
          (Util.getLengthOfTree(line) - 1, None)
        //definitions to skip over (object, class, function)
        case q"$modifs object $tname extends { ..$earlydefins } with ..$pparents { $sself => ..$body }" =>
          (Util.getLengthOfTree(line) - 1, None)
        case q"$mod class $pname[..$tpara] $actMods(...$para) extends { ..$defs } with ..$prnts { $self => ..$sts }" =>
          (Util.getLengthOfTree(line) - 1, None)
        case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" =>
          (Util.getLengthOfTree(line) - 1, None)
        //assignment
        case q"val $assignee = $newValue" =>
          /*_*/ processNovelAssignment(assignee.toString, assignee.tpe, newValue) /*_*/
          (Util.getLengthOfTree(line) - 1, None)
        case q"var $assignee = $newValue" =>
          /*_*/ processNovelAssignment(assignee.toString, assignee.tpe, newValue) /*_*/
          (Util.getLengthOfTree(line) - 1, None)
        case q"$assignee = $newValue" =>
          processAssignment(assignee.toString, assignee.tpe, newValue)
          (Util.getLengthOfTree(line) - 1, None)
        //for loops
        case q"for (..$generator) $loopBody" =>
          dealWithLoop(LoopType.forLoop, loopBody, enums = generator)
          (Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        case q"for (..$generator) yield $loopBody" =>
          dealWithLoop(LoopType.forLoop, loopBody, enums = generator)
          (Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        //while(true) and dowhile(true)
        case LabelDef(TermName(name), List(), block@Block(statements, Apply(Ident(TermName(name2)), List())))
          if (name.startsWith("while$") || name.startsWith("doWhile$")) && name2 == name =>
          dealWithLoop(LoopType.trueLoop, block.asInstanceOf[Trees#Tree])
          (Set(), Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        //while loops
        case q"while ($cond) $loopContents" =>
          dealWithLoop(LoopType.whileLoop, loopContents, cond = cond)
          (Util.getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        case q"do $loopContents while ($cond)" =>
          dealWithLoop(LoopType.dowhileLoop, loopContents, cond = cond)
          (0, None)
        //Functions (first is for functions defined in the same scope, second the others)
        case func@Apply(Ident(functionName), args) =>
          var copiedMap = copyMap(protocolledElements)
          val returned = dealWithFunction(func, functionName, args)
          updateStateIfNeeded(copiedMap, line)
          (Util.getLengthOfTree(line) - 1, returned) //because we are processing the current one already
        case func@Apply(Select(instanceCalledOn, functionName), args) =>
          var copiedMap = copyMap(protocolledElements)
          val returned = dealWithFunction(func, functionName, args, instanceCalledOn)
          updateStateIfNeeded(copiedMap, line)
          (Util.getLengthOfTree(line) - 1, returned) //because we are processing the current one already
        case q"if ($cond) $ifBody else $elseBody" =>
          val returned = dealWithIfElse(cond, ifBody, elseBody)
          (Util.getLengthOfTree(line) - 1, returned)
        case q"try $tryBody catch { case ..$cases } finally $finallyBody" =>
          /*_*/ checkTryCatchFinally(tryBody, finallyBody) /*_*/
          (Util.getLengthOfTree(line) - 1, None)
        case q"$expr match { case ..$cases }" =>
          /*_*/ processMatchStatement(expr, cases) /*_*/
          (Util.getLengthOfTree(line) - 1, None)

        //All three next cases are to check for solitary object name on a line
        case Ident(TermName(objectName)) =>
          checkObject(objectName)
          getClosestScopeAliasInfo(objectName) match {
            case Some(aliasInfo) =>
              val returned = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (0, Some(returned))
            case None =>
              (0, None)
          }
        case Select(location, expr) =>
          var exprString = expr.toString()
          if (exprString.lastIndexOf(".") != -1)
            exprString = exprString.substring(exprString.lastIndexOf(".") + 1)
          checkObject(exprString)
          getClosestScopeAliasInfo(exprString.trim) match {
            case Some(aliasInfo) =>
              val returned = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (0, Some(returned))
            case None =>
              (0, None)
          }
        case Block(List(expr), Literal(Constant(()))) =>
          var exprString = expr.toString()
          if (exprString.lastIndexOf(".") != -1)
            exprString = exprString.substring(exprString.lastIndexOf(".") + 1)
          checkObject(exprString)
          getClosestScopeAliasInfo(exprString.trim) match {
            case Some(aliasInfo) =>
              val returned = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (0, Some(returned))
            case None =>
              (0, None)
          }
        case _ =>
          (0, None)
      }
    }

    private def dealWithBreak(label: String) = {
      for ((elementName, savedInstances) <- savedBreakInstances) {
        if (savedInstances.contains(label)) {
          savedInstances(label) += copyInstances(protocolledElements(elementName).instances)
        } else
          savedInstances += (label -> ArrayBuffer(copyInstances(protocolledElements(elementName).instances)))
        println(s"after dealing with break with label $label, saved instances are " + savedInstances)
      }
    }

    def dealWithBreakable(label:Name, body:Seq[Trees#Tree]) = {
      println("In breakable with label " + label)
      checkInsideObjectBody(body)
      println("merging instances at the end of breakable with label "+label)
      for ((elementName, savedInstances) <- savedBreakInstances) {
        if (savedInstances.contains(label.toString())) {
          for (instances <- savedInstances(label.toString()))
            protocolledElements(elementName).instances =
              mergeInstanceStates(protocolledElements(elementName).instances, instances)
        }
        println("at the end of breakable, instances are " + protocolledElements(elementName).instances)
        savedInstances.remove(label.toString())
      }
    }

    def processAssignment(assignee: String, assigneeType: Type, assigned: Trees#Tree) = {
      val returnedAssigned = checkInsideFunctionBody(assigned)
      getClosestScopeAliasInfo(assignee, assigneeType) match {
        case Some(assigneeAliasInfo) =>
          returnedAssigned match {
            case Some(returned) =>
              var returnedInstances = Util.copyInstances(returned)
              var scopesToRemove: ArrayBuffer[mutable.Stack[String]] = ArrayBuffer()
              removeAliases(assigneeType, assigneeAliasInfo._1)
              for (instance <- returnedInstances) {
                if (instance.containsScopeAlias())
                  scopesToRemove += instance.aliases.last.scope
                else {
                  if (protocolledElements(assigneeType).instances.contains(instance)) {
                    for (existingInstance <- protocolledElements(assigneeType).instances) {
                      if (instance == existingInstance)
                        existingInstance.aliases += Alias(assigneeAliasInfo._1, assigneeAliasInfo._2)
                    }
                  }
                  else {
                    instance.aliases += Alias(assigneeAliasInfo._1, assigneeAliasInfo._2)
                    protocolledElements(assigneeType).instances += instance
                  }
                }
              }
              for (scopeToRemove <- scopesToRemove) {
                removeAllAliasesInScope(scopeToRemove)
              }
            case None =>
          }
        case None =>
      }
    }


    /** Processes a val assignee = assigned statement
     * Checks if assigned is an existing alias and if so adds assignee to its list of aliases
     *
     * @param assignee  In val/var x = y, this is x. Always comes as a new val or var.
     * @param assigned  In val/var x = y, this is y.
     * @param instances The instances passed in to process the assignment with.
     * @return
     */
    def processNovelAssignment(assignee: String, assigneeType: Type, assigned: Trees#Tree) = {
      var returnedAssigned = checkInsideFunctionBody(assigned)
      returnedAssigned match {
        case Some(returned) =>
          var returnedInstances = Util.copyInstances(returned)
          var scopesToRemove: ArrayBuffer[mutable.Stack[String]] = ArrayBuffer()
          for (instance <- returnedInstances) {
            if (instance.containsScopeAlias()) {
              scopesToRemove += instance.aliases.last.scope
            } else {
              if (protocolledElements(assigneeType).instances.contains(instance)) {
                for (existingInstance <- protocolledElements(assigneeType).instances) {
                  if (instance == existingInstance)
                    existingInstance.aliases += Alias(assignee, Util.currentScope.clone())
                }
              }
              else {
                instance.aliases += Alias(assignee, Util.currentScope.clone())
                protocolledElements(assigneeType).instances += instance
              }
            }
          }
          for (scopeToRemove <- scopesToRemove) {
            removeAllAliasesInScope(scopeToRemove)
          }
        case None =>
      }
    }


    def isAliasCallingProtocolMethod(expr: Trees#Tree, instances:Set[Instance]):Boolean =  {
      println("expr is "+expr)
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

    def processMatchStatement(expr: Trees#Tree, cases: List[CaseDef])= {
      if(!isAliasCallingProtocolMethod(expr)) {
        val returned = checkInsideFunctionBody(expr) //have this as two lines because just ._1 wasn't working here, oddly
        processCaseStatements(cases)
      }
      else{
        println("is alias calling protocol method")
        processCaseStatements(cases, expr)
      }
    }

    def processCaseStatements(cases: List[global.CaseDef], expr:Trees#Tree=null) = {
      //copy instances into newInstances properly like in if/else code
      var caseInstances: Set[Instance] = Set()
      for (instance <- instances) caseInstances += Instance(instance.className, instance.aliases, instance.currentStates)
      //this needs to actually process what is inside the case statement rather than the entire statement
      if(expr != null){
        println("expr is not null")
        var returnValue = cases.head.pat.toString()
        println("here, return value is "+returnValue)
        if(returnValue.contains("."))
          returnValue = returnValue.substring(returnValue.lastIndexOf('.')+1)
        caseInstances = updateStateIfNeeded(caseInstances, caseInstances, expr, ":"+returnValue)
      }
      val newInstances = checkInsideFunctionBody(cases.head.body, caseInstances)._1
      if (cases.tail.nonEmpty)
        Util.mergeInstanceStates(newInstances, processCaseStatements(cases.tail, expr))
      else
        newInstances
    }

    object LoopType extends Enumeration{
      type LoopType = Value
      val forLoop, whileLoop, dowhileLoop, trueLoop = Value
    }

    def dealWithLoop(loopType: LoopType.Value, loopContent: Trees#Tree, enums: Seq[Trees#Tree]=null, cond: Trees#Tree=null) = {
      //initialisations
      var instanceToInterimStates: mutable.HashMap[Type, mutable.HashMap[Set[Alias], ListBuffer[Set[State]]]] = mutable.HashMap()
      for((elementType, elementInfo) <- protocolledElements){
        instanceToInterimStates += (elementType -> null)
        if(loopType == LoopType.dowhileLoop || loopType == LoopType.trueLoop) //initialise the list to empty since these loops will always execute at least once
          for(instance <- elementInfo.instances) instanceToInterimStates(elementType) += instance.aliases -> ListBuffer()
        else
          for(instance <- elementInfo.instances) instanceToInterimStates(elementType) += instance.aliases -> ListBuffer(instance.currentStates)
      }
      //loop
      do {
        loopType match{
          case LoopType.forLoop =>
            checkInsideForLoopGenerator(enums)
            Util.currentScope.push("for")
          case LoopType.whileLoop =>
            checkInsideFunctionBody(cond)
            Util.currentScope.push("while")
          case LoopType.dowhileLoop =>
            Util.currentScope.push("dowhile")
          case LoopType.trueLoop =>
            Util.currentScope.push("true")
        }
        //go through loop body
        checkInsideFunctionBody(loopContent)
        removeAllAliasesInScope(Util.currentScope)
        Util.currentScope.pop()
        if(loopType == LoopType.dowhileLoop)
          checkInsideFunctionBody(cond)
        //update map with new states of the instances
        for((elementType, elementInfo) <- protocolledElements){
          if(instanceToInterimStates.contains(elementType)){
            for (instance <- elementInfo.instances if instanceToInterimStates(elementType).contains(instance.aliases))
              instanceToInterimStates(elementType)(instance.aliases) += instance.currentStates
          }
        }
        println("right before check for duplicates, map is " + instanceToInterimStates)
      } while (!duplicatesInAllListsOfMap(instanceToInterimStates))
      if(loopType == LoopType.whileLoop)
        checkInsideFunctionBody(cond) //go through the while loop condition one more time after the body of the loop
      //assigns interim states to the instances
      for(elementType <- instanceToInterimStates.keys)
        for((savedAliases, setOfStates) <- instanceToInterimStates(elementType))
          for(instance <- protocolledElements(elementType).instances if instance.aliases == savedAliases)
            instance.currentStates ++= setOfStates
    }

    def checkInsideForLoopGenerator(enums: Seq[Trees#Tree])= {
      enums match {
        case List(fq"$pat <- $beginning until $end") =>
          for (begin <- beginning.children)
            checkInsideFunctionBody(begin)
          checkInsideFunctionBody(end)
        case List(fq"$pat <- $beginning to $end") =>
          for (begin <- beginning.children)
            checkInsideFunctionBody(begin)
          checkInsideFunctionBody(end)
        case List(fq"$pat <- $gen") =>
          checkInsideFunctionBody(gen)
        case _ => checkInsideObjectBody(enums)
      }
    }

    /** Handles try-catch in a basic manner, assuming no exceptions.
     * Just goes through try then finally bodies.
     *
     * @param tryBody     Code inside the try block.
     * @param cases       List of cases.
     * @param finallyBody Code inside the finally block.
     * @return
     */
    def checkTryCatchFinally(tryBody: Trees#Tree, finallyBody: Trees#Tree) = {
      checkInsideFunctionBody(tryBody)
      checkInsideFunctionBody(finallyBody)
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
     * @return
     */
    def getClosestScopeAliasInfo(name: String, elementType:Type): Option[(String, mutable.Stack[String])] = {
      if(protocolledElements.contains(elementType)) {
        if (protocolledElements(elementType).instances.isEmpty) return None
        val curScope = Util.currentScope.clone()
          while (curScope.nonEmpty) {
            for (instance <- protocolledElements(elementType).instances) {
              for (alias <- instance.aliases if alias.name == name && alias.scope == curScope) {
                return Some(alias.name, alias.scope)
              }
            }
            curScope.pop()
          }
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
          if (element.elementType.toString() == objectName && element.scope == curScope) {
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
    def checkObjectFunctionCall(calledOn: global.Tree): Unit = {
      if (calledOn == null) return
      var calledOnType = calledOn.tpe
      for (element <- classAndObjectTraverser.classesAndObjects
           if (!element.initialised && element.isObject && element.elementType == calledOnType
             && element.scope == getScope(calledOn))) {
        element.initialised = true
        Util.currentScope.push(calledOn.toString())
        checkInsideObjectBody(element.body)
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

    def isAssignmentFunction(funcCall: global.Apply): Boolean = {
      funcCall match {
        case Apply(Select(arg1, TermName(functionName)), List(arg3)) =>
          val regex = ".*_\\$eq".r
          regex.findPrefixMatchOf(functionName) match {
            case Some(mat) =>
              val value = mat.toString
              processAssignment(value.substring(0, value.length - 4), arg3)
              return true
            case None =>
          }
        case _ =>
      }
      false
    }

    def assignParameters(parameters: ArrayBuffer[(String, Type)], args: List[Tree], calledOn: Tree) = {
      if (calledOn != null && protocolledElements.contains(calledOn.symbol.tpe)) {
        processNovelAssignment(calledOn.symbol.tpe.toString(), calledOn.symbol.tpe, calledOn)
      }
      var i = 0
      for (param <- parameters) {
          if (protocolledElements.contains(param._2)) {
            processNovelAssignment(param._1, param._2, args(i))
          }
        i += 1
      }
    }

    def checkArguments(args: List[global.Tree]) = {
      for (arg <- args) {
        checkInsideFunctionBody(arg)
      }
    }

    def cacheContainsCurrentStates(map: Map[ArrayBuffer[(Type, Set[String], Set[State])], ArrayBuffer[Set[State]]],
                                   array2: ArrayBuffer[(Type, Set[String], Set[State])]):
    (Boolean, ArrayBuffer[(Type, Set[String], Set[State])]) = {
      for ((array, states) <- map) {
        val set1 = array.toSet
        val set2 = array2.toSet
        if (set1.equals(set2)) return (true, array)
      }
      (false, null)
    }

    def createCacheEntry(givenToFunctionParams: mutable.HashMap[Type, mutable.HashMap[(String, mutable.Stack[String]),
      Set[(String, mutable.Stack[String])]]]): ArrayBuffer[(Type, Set[String], Set[State])] = {
      val cacheEntry = ArrayBuffer[(Type, Set[String], Set[State])]()
      for(elementType <- givenToFunctionParams.keys) {
        for ((aliasInfo, paramInfos) <- givenToFunctionParams(elementType)) {
          var currentStates = Set[State]()
          for (paramInfo <- paramInfos) {
            val instancesToGetStatesFrom = protocolledElements(elementType).instances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
            for (instance <- instancesToGetStatesFrom) {
              currentStates ++= instance.currentStates
            }
          }
          var paramNames = Set[String]()
          for (paramInfo <- paramInfos) paramNames += paramInfo._1
          cacheEntry.append((elementType, paramNames, currentStates))
        }
      }
      cacheEntry
    }

    def mutateInstances(parameters: ArrayBuffer[(Type, Set[String], Set[State])], function: Function) = {
      var i = 0
      for ((elementType, parameterNames, states) <- parameters) {
        for (paramName <- parameterNames) {
          getClosestScopeAliasInfo(paramName, elementType) match {
            case Some(paramInfo) =>
              val instancesToMutate = protocolledElements(elementType).instances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
              for (instance <- instancesToMutate) instance.currentStates = function.stateCache(parameters)(i)
            case None =>
          }
        }
        i += 1
      }
    }

    def findNextStates(cacheEntry: ArrayBuffer[(Type, Set[String], Set[State])]): ArrayBuffer[Set[State]] = {
      val nextStatesArray = ArrayBuffer[Set[State]]()
      for ((elementType, parameterNames, states) <- cacheEntry) {
        var nextStates = Set[State]()
        for (paramName <- parameterNames) {
          getClosestScopeAliasInfo(paramName, elementType) match {
            case Some(paramInfo) =>
              val instancesWithNextStates = protocolledElements(elementType).instances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
              for (instance <- instancesWithNextStates) nextStates ++= instance.currentStates
            case None =>
          }
        }
        nextStatesArray.append(nextStates)
      }
      nextStatesArray
    }

    /** Checks if two lists of parameters, formatted differently, are the same
     *
     * @param firstParameters ArrayBuffer of parameters as arrays with form [
     * @param secondParameters
     * @return
     */
    def typesMatch(firstParameters:ArrayBuffer[(String, Type)], secondParameters:List[global.Tree]): Boolean ={
      if(!firstParameters.exists(param => param._1.length > 0) && secondParameters.isEmpty) return true //both lists are empty
      for((param, i) <- firstParameters.zipWithIndex) {
        if(param._2 != secondParameters(i).tpe)
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
     * @param calledOn
     * @return
     */
    def dealWithFunction(funcCall: global.Apply, functionName: global.Name, args: List[global.Tree], calledOn: Tree = null):
    Option[Set[Instance]] = {
      //region <Checks>

      //check for an assignment function, don't want to check args or do anything else in this case
      if (isAssignmentFunction(funcCall)) return None

      //checks and gets parameters
      checkArguments(args)

      //checks for "new Class()" constructor function
      val isCurrentIsNewAndReturned = checkNewFunction(funcCall, args)
      if (isCurrentIsNewAndReturned._2) {
        if (isCurrentIsNewAndReturned._1)
          return Some(isCurrentIsNewAndReturned._3)
        else return None
      }
      //checks for Object constructor call
      checkObjectFunctionCall(calledOn)
      //endregion
      //finding function definition
      for (function <- functionTraverser.functions
           if (function.name == functionName.toString()
             && function.scope == getScope(funcCall, dontCheckSymbolField = true)
             && typesMatch(function.params, args))) {
        println("found function " + function.name)
        Util.currentScope.push(function.name) //push scope so parameters will be scoped inside the function
        assignParameters(function.params, args, calledOn)
        //create maps
        val givenToFunctionParams= createMap(function.params)
        //make possible cache entry
        val cacheEntry = createCacheEntry(givenToFunctionParams)
        //check if it hits
        val cacheHitAndParams = cacheContainsCurrentStates(function.stateCache, cacheEntry)
        val cacheHit = cacheHitAndParams._1
        val parameters = cacheHitAndParams._2
        //mutate state if possible and skip recursive call if needed
        if (cacheHit && function.stateCache(parameters) != null) {
          mutateInstances(parameters, function)
          removeAllAliasesInScope(Util.currentScope)
          println("skipping")
          Util.currentScope.pop()
          return function.returned
        }
        if (cacheHit && function.stateCache(parameters) == null) {
          removeAllAliasesInScope(Util.currentScope)
          println("recursing")
          Util.currentScope.pop()
          return function.returned
        }
        //if it doesn't, put in a null entry
        function.stateCache += cacheEntry -> null
        //check inside the function body
        Util.currentScope.push("body")
        val returned = checkInsideFunctionBody(function.body)
        //figuring out what is returned PUT THIS INTO ITS OWN FUNCTION
        returned match {
          case Some(setOfInstances) =>
            var scopeClone = Util.currentScope.clone()
            var instancesReturned = setOfInstances ++ Set(Instance(Set(Alias("scope+", scopeClone.clone())), Set())) //delete internal variables
            scopeClone.pop()
            instancesReturned = instancesReturned ++ Set(Instance(Set(Alias("scope+", scopeClone)), Set())) //need to delete the parameters too
            function.returned = Some(Util.copyInstances(instancesReturned))
          case None =>
            function.returned = Some(Set(Instance(Set(Alias("scope+", Util.currentScope.clone())), Set())))
        }
        //remove aliases inside the body of the function since they can't be used anymore
        removeAllAliasesInScope(Util.currentScope)
        Util.currentScope.pop()
        //construct array of next states
        val nextStates = findNextStates(cacheEntry)
        //update cache
        function.stateCache += cacheEntry -> nextStates
        //delete aliases in function here
        removeAllAliasesInScope(Util.currentScope)
        println(s"the deal with function returns ${function.returned}")
        Util.currentScope.pop()
        return function.returned
      }
      None
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
          checkInsideObjectBody(obj.body)
          Util.currentScope.pop()
        case _ =>
      }
      newInstances
    }


    /** Checks for a new x function and executes the code within the class if found. Renames instances to
     * constructor parameter names if needed.
     *
     * @param funcCall
     */
    def checkNewFunction(funcCall: global.Apply, args: List[Tree]): (Boolean, Boolean, Set[Instance]) = {
      var isCurrentType = false
      var returned: Set[Instance] = Set()
      funcCall match {
        case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          parents match {
            case List(item) =>
              //note here getScope is called on the symbol owner of funcCall since we want to skip the class name in the scope
              val IsCurrentTypeAndReturned = checkInsideClass(getScope(funcCall.symbol.owner),item.tpe , args)
              isCurrentType = IsCurrentTypeAndReturned._1
              returned = IsCurrentTypeAndReturned._2
            case List(Apply(className, arg2)) =>
              val IsCurrentTypeAndReturned = checkInsideClass(getScope(className), className.toString(), args)
              isCurrentType = IsCurrentTypeAndReturned._1
              returned = IsCurrentTypeAndReturned._2
            case _ =>
          }
          return (isCurrentType, true, returned)
        case _ =>
      }
      (false, false, returned)
    }

    def checkInsideClass(scope: mutable.Stack[String], elementType: Type, args: List[Tree]): (Boolean, Set[Instance]) = {
      var returned: Set[Instance] = null
      for (element <- classAndObjectTraverser.classesAndObjects
           if !element.isObject && element.elementType == elementType && element.scope == scope) {
        assignParameters(element.params, args, null)
        Util.currentScope.push(elementType.toString())
        protocolledElements(elementType).instances +=
          Instance(Set(Alias(elementType.toString(), Util.currentScope.clone())), Set(currentElementInfo.states(0)))
        checkInsideObjectBody(element.body)
        val scopeInstance = Instance(Set(Alias("scope+", Util.currentScope.clone())), Set())
        for (instance <- protocolledElements(elementType).instances) {
          if (instance.containsAliasInfo(elementType.toString(), Util.currentScope))
            returned = Set(Instance(instance.aliases, instance.currentStates), scopeInstance)
        }
        removeAllAliasesInScope(Util.currentScope)
        Util.currentScope.pop()
      }
      (protocolledElements.contains(elementType), returned)
    }

    /** For the parameter list given, checks if they match any of our defined instances. If so, renames the instance
     * to the parameter name. Keeps memory of the renaming in a hashmap of parameter name to instance name so these
     * can easily be renamed after the function exits.
     *
     * @return
     */
    def createMap(parameters: ArrayBuffer[(String, Type)]):
    mutable.HashMap[Type,mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]] = {
      var givenToFunctionParam = new mutable.HashMap[Type, mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]]()
      //construct maps
      var argCounter = 0
      val paramScope = Util.currentScope.clone()
      for (arg <- parameters) {
        val argName = arg._1
        getClosestScopeAliasInfo(argName, arg._2) match {
          case Some(aliasInfo) =>
            val paramName = parameters(argCounter)._1
            givenToFunctionParam(arg._2).get(aliasInfo._1, aliasInfo._2) match {
              case Some(setOfParams) =>
                val updatedSet = setOfParams ++ Set((paramName, paramScope))
                givenToFunctionParam(arg._2) += (aliasInfo._1, aliasInfo._2) -> updatedSet
              case None =>
                givenToFunctionParam(arg._2) += (aliasInfo._1, aliasInfo._2) -> Set((paramName, paramScope))
            }
          case None =>
        }
        argCounter += 1
      }
      givenToFunctionParam
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
        println("return value is "+returnValue)
        var methodName = methodCallInfo(0)+returnValue
        println("before stripping, method name is "+methodName)
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
            classesAndObjects += ClassOrObject(tname.tpe, ArrayBuffer(), body, getScope(obj), isObject = true)
            super.traverse(obj)
          case cla@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
            val parameters = /*_*/ getParametersWithInstanceNames(paramss) /*_*/
            classesAndObjects += ClassOrObject(tpname.tpe, parameters, stats, getScope(cla))
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
    def removeAliases(elementType:Type, aliasName: String): Set[Instance] = {
      getClosestScopeAliasInfo(aliasName, elementType) match {
        case Some(aliasInfo) =>
          val instancesToUpdate = protocolledElements(elementType).instances.filter(instance =>
            instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
          for (instance <- instancesToUpdate)
            instance.aliases -= Alias(aliasInfo._1, aliasInfo._2)
        case None =>
      }
      protocolledElements(elementType).instances = cleanInstances(protocolledElements(elementType).instances)
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
    def checkProtocolMethodsSubsetClassMethods(returnValuesArray: Array[ReturnValue], stats: Seq[Trees#Tree], elementType: Type, filename: String): Unit = {
      val classMethods = getMethodNames(stats)
      var protocolMethods: Set[String] = Set()
      for (i <- returnValuesArray.indices) {
        protocolMethods += Util.stripReturnValue(returnValuesArray(i).parentMethod.name.replaceAll("\\s", ""))
      }
      if (!(protocolMethods subsetOf classMethods)) throw new badlyDefinedProtocolException(
        s"Methods $protocolMethods defined in $filename are not a subset of methods " +
          s"$classMethods defined in class $elementType. Methods ${protocolMethods.diff(classMethods)} are defined in " +
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
            Some(arg2.head.toString().stripSuffix(""""""").stripPrefix("""""""))
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
    def getParametersWithInstanceNames(params: List[List[ValDef]]): ArrayBuffer[(String, Type)] = {
      params match {
        case List(List()) => ArrayBuffer(Array(""))
        case List(List(value)) => ArrayBuffer(Array(value.name.toString(), value.tpt))
        case List(values) =>
          var parameters: ArrayBuffer[(String, Type)] = ArrayBuffer()
          for (elem <- values) {
            parameters += Array(elem.name.toString(), elem.tpt)
          }
          parameters
        case _ => ArrayBuffer()
      }
    }


    /** Checks to see if there are duplicates in all the lists of a map(Instance -> list) */
    def duplicatesInAllListsOfMap(maps:mutable.HashMap[Type, mutable.HashMap[Set[Alias], ListBuffer[Set[State]]]]):Boolean={
      for(map <- maps.values)
        for((instance, list) <- map) for((instance, list) <- map if list.diff(list.distinct).isEmpty) return false
      true
    }

    def copyMap(map: mutable.Map[Type, ElementInfo]): mutable.Map[Type, ElementInfo] ={
      var copiedMap = mutable.Map[Type, ElementInfo]()
      for((elementType, elementInfo) <- map){
        copiedMap += (elementType.clone() -> ElementInfo(elementInfo.transitions, elementInfo.states,
          elementInfo.methodToIndices, elementInfo.returnValueToIndice, copyInstances(elementInfo.instances)))
      }
      copiedMap
    }


    def removeAllAliasesInScope(scope:mutable.Stack[String]) = {
      breakable {
        if (scope == null) break()
        for ((elementType, elementInfo) <- protocolledElements) {
          for (instance <- elementInfo.instances) {
            for (alias <- instance.aliases) {
              if (alias.scope == scope)
                instance.aliases -= alias
            }
          }
          cleanInstances(elementInfo.instances)
        }
      }
    }

  }
}







