package compilerPlugin

import ProtocolDSL.{ReturnValue, State}
import compilerPlugin.Util._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{SortedSet, mutable}
import scala.reflect.api.Trees
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}
import scala.util.control.Breaks._


/** My plugin */
class GetFileFromAnnotation(val global: Global) extends Plugin {
  val name = "GetFileFromAnnotation"
  val description = "Checks the protocol defined on a class or object with the Typestate annotation"
  lazy val components =
    new MyComponent(global) :: Nil

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    for (option <- options) {
      userDirectory = option
    }
  }
}

/** The component which will run when my plugin is used */
class MyComponent(val global: Global) extends PluginComponent {

  import global._

  /** Holds information about a function in the code
   *
   * @param name       : Name of the function
   * @param params     : The parameters of the function, stored in an array with name, type
   * @param returnType : The return type of the function, if it returns anything
   * @param body       : The body of the function (the code inside of it)
   * @param scope      : The scope of the function
   * @param stateCache : contains a mapping of parameters (elementType, parameterNames, statesBefore) to nextStates.
   *                   There can be several parameter names if a function is called with duplicate values as arguments.
   * @param returned   : The instances if any, that this function returns
   */
  case class Function(name: String, owner: Symbol, params: ArrayBuffer[(String, String)],
                      returnType: Trees#Tree, body: Trees#Tree, scope: mutable.Stack[String],
                      var stateCache: Map[mutable.Map[String, ElementInfo], mutable.Map[String, ElementInfo]],
                      var returned: Option[Set[Instance]]) { //might replace string with elementInfo for more precision (on build)
    override def toString: String = {
      s"name: $name parameters: $params return type: $returnType scope: $scope body: $body stateCache: $stateCache returned: $returned"
    }
  }

  /** Holds information about an element (a class or object)
   *
   * @param elementType : type of the element, this gives a more precise way of identifying the element than just the name
   * @param params      : parameters given to initialise a class. Not applicable to objects
   * @param body        : body of the element
   * @param scope       : scope of the element
   * @param isObject    : true if the element is an object, false if it is a class. Defaults to false
   * @param initialised : only relevant if the element is an object. Keeps track of if this object has been initialised
   */
  case class Element(elementType: String, owner:Symbol, params: ArrayBuffer[(String, String)], body: Seq[Trees#Tree], scope: mutable.Stack[String],
                     isObject: Boolean = false, var initialised: Boolean = false, var id:Int=0) {
    override def toString(): String = {
      s"$elementType ${showParams(params)} $scope $initialised"
    }

    def showParams(params: ArrayBuffer[(String, String)]): String = {
      var parameters = ""
      for (param <- params) {
        parameters += param._1 + ": " + param._2 + " ; "
      }
      parameters
    }
  }

  val runsAfter: List[String] = List[String]("refchecks")
  val phaseName: String = "compilerPlugin.GetFileFromAnnotation.this.name"

  def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)

  /** Phase which is run by the plugin */
  class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
    override def name: String = "compilerPlugin.GetFileFromAnnotation.this.name"

    var compilationUnit: CompilationUnit = _

    var userDirectory:String = _



    /** Used to differentiate between different types of loop.
     * Contains values: forLoop, whileLoop, dowhileLoop and trueLoop (for while(true) or dowhile(true) loops)
     *
     */
    object LoopType extends Enumeration {
      type LoopType = Value
      val forLoop, whileLoop, dowhileLoop, trueLoop = Value
    }


    /** Entry point of the plugin.
     * In a first pass, goes through the code collecting element and function information.
     * Then it finds all the elements which have a Typestate annotation and creates an entry in the protocolledElemenets
     * map for each of them.
     * It then checks the code for protocol violations.
     *
     * @param unit : contains tree of the code in body
     */
    def apply(unit: CompilationUnit): Unit = {
      
      //
      
      compilationUnit = unit
      functionTraverser.traverse(unit.body)
      ElementTraverser.traverse(unit.body)
      checkFile()
    }

    def checkAllProtocolledInstancesAreInEndState() = {
      for(elementInfo <- trackedElements.values if elementInfo.states != null){
        for(instance <- elementInfo.instances if !(instance.currentStates.size == 1 && instance.currentStates.last.name == "end"))
          throw new unendedProtocolException(instance.alias.name, sortSet(getFieldNamesPointingAtInstance(instance)),
            sortSet(instance.currentStates))
      }
    }

    /** Checks that a class or object is following its protocol
     * Goes through the code to find either the object with App or the main function and thereby gets the entrypoint
     * of the code and can start analysing it from there.
     *
     * Limited at the moment
     *
     * */
    def checkFile(): Unit = {
      for (line@q"$mods object $objectName extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
        breakable {
          val objectType = line.symbol.tpe.toString()
          for (parent <- parents) {
            if (parent.toString() == "App") {
              currentScope = getScope(line)
              for (element <- ElementTraverser.elements if
                (element.elementType == objectType && element.scope == getScope(line)))
                element.initialised = true
              currentScope.push(objectType)
              initObjects()
              currentInstance.push(
                trackedElements(objectType).instances.filter(instance => instance.containsAliasInfo(objectType, currentScope)).last)
              if(currentInstance.nonEmpty)
                checkInsideObjectBody(body) //all the code is checked here
              if(currentInstance.nonEmpty) currentInstance.pop()
              
              break
            }
          }
          for (definition <- body) {
            definition match {
              case mainLine@q"$mods def main[..$tparams](...$paramss): $tpt = $expr" =>
                currentScope = getScope(line)
                /*_*/
                if (getParameterTypes(paramss) == "Array[String]") {
                  /*_*/
                  
                  currentScope.push(objectType)
                  initObjects()
                  currentScope.pop()
                  checkObject(objectType)
                  currentScope.push(objectType)
                  currentScope.push("main")
                  
                  getClosestScopeAliasInfo(objectType, objectType) match{
                    case Some(aliasInfo) =>
                      currentInstance.push(trackedElements(objectType).instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2)).head)
                    case None =>
                  }
                  if(currentInstance.nonEmpty)
                    checkInsideFunctionBody(expr) //all the code is checked here
                  currentScope.pop()
                  currentScope.pop()
                }
              case _ =>
            }
          }
        }
      }
      checkAllProtocolledInstancesAreInEndState()
      //trackedElements = mutable.Map[String,compilerPlugin.ElementInfo]()
    }


    /** Goes inside an object to see if there are instances with protocols and if they are following their protocol
     * Analyses the code line by line with checkInsideFunctionBody.
     *
     * @param code The code to check
     */
    def checkInsideObjectBody(code: Seq[Trees#Tree]) = {
      for (line <- code) checkInsideFunctionBody(line)
    }


    /** Checks the code inside a function for protocol violations.
     * Goes line by line and skips lines if needed (for example when at a definition or a loop).
     * Returns a set of instances wrapped in an option
     *
     * @param code The code to check
     * @return What is returned from the function, if applicable
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
          if (nbOfLinesToSkip == -1) //indicates a break statement has just been processed
            return None
          if (NbLinesToSkipAndReturned._2 == null || NbLinesToSkipAndReturned._2.isDefined) // do this to avoid erasing returned element
            returned = NbLinesToSkipAndReturned._2
        }
      }
      returned
    }


    /** Checks a line and updates instances if needed.
     * Returns a set of instances if relevant.
     * Has different cases for different types of line.
     *
     * @param line line of code to analyse
     * @return Set of instances returned by the line
     */
    def processLine(line: Trees#Tree): (Int, Option[Set[Instance]]) = {
      println(s"processing line $line")
      println("tracked are "+trackedElements)
      val previousCurInstance= currentInstance.pop()
      val candidateCurInstance = getInstanceWithTypeId(previousCurInstance.alias.name, previousCurInstance.id)
      if(candidateCurInstance == null) {
        val instanceToPush = previousCurInstance
        instanceToPush.fields = mutable.Map[Alias, Set[Instance]]()
        currentInstance.push(instanceToPush)
      } else
        currentInstance.push(candidateCurInstance)
      printInstances()
      //special case for _ pattern as it can't be matched with case statements
      if (line == EmptyTree.asInstanceOf[Tree]) {
        return (0, null)
      }
      line match {
        //definitions to skip over (object, class, function)
        case q"$modifs object $tname extends { ..$earlydefins } with ..$pparents { $sself => ..$body }" =>
          (getLengthOfTree(line) - 1, None)
        case q"$mod class $pname[..$tpara] $actMods(...$para) extends { ..$defs } with ..$prnts { $self => ..$sts }" =>
          (getLengthOfTree(line) - 1, None)
        case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" =>
          (getLengthOfTree(line) - 1, None)
        //break and breakable
        case Apply(Select(Select(scope, label), TermName("break")), body) =>
          dealWithBreak(label.toString())
          removeAllInstances()
          (-1, None)
        case Apply(Select(Ident(TermName(label)), TermName("break")), List()) =>
          dealWithBreak(label)
          removeAllInstances()
          (-1, None)
        case Apply(Select(Select(scope, label), TermName("breakable")), body) =>
          dealWithBreakable(label, body)
          println("after breakable, tracked are "+trackedElements)
          (getLengthOfTree(line) - 1, None)
        case Apply(Select(Ident(label), TermName("breakable")), body) =>
          dealWithBreakable(label, body)
          println("after breakable, tracked are "+trackedElements)
          (getLengthOfTree(line) - 1, None)
        //assignments
        case ValDef(mods, TermName(assignee), assigneeType, Literal(Constant(null))) =>
          processNovelAssignment(assignee, Literal(Constant(null)).asInstanceOf[Tree])
          (getLengthOfTree(line) - 1, None)
        case ValDef(mods, TermName(assignee), assigneeType, EmptyTree) =>
          processNovelAssignment(assignee, EmptyTree.asInstanceOf[Tree])
          (getLengthOfTree(line) - 1, None)
        case ValDef(mods, TermName(assignee), assigneeType, assigned) =>
          /*_*/ processNovelAssignment(assignee, assigned) /*_*/
          (getLengthOfTree(line) - 1, None)
        case q"val $assignee = $newValue" =>
          /*_*/ processNovelAssignment(assignee.toString, newValue) /*_*/
          (getLengthOfTree(line) - 1, None)
        case q"var $assignee = $newValue" =>
          /*_*/ processNovelAssignment(assignee.toString, newValue) /*_*/
          (getLengthOfTree(line) - 1, None)
        case q"$assignee = $newValue" =>
          /*_*/ getFields(assignee) /*_*/ match{
            case Some(fields) =>
              processAssignment(fields, newValue)
            case None =>
              processAssignment(mutable.Stack((assignee.toString, newValue.tpe.toString)), newValue)
          }
          (getLengthOfTree(line) - 1, None)
        //for loops
        case q"for (..$generator) $loopBody" =>
          dealWithLoop(LoopType.forLoop, loopBody, generator = generator)
          (getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        case q"for (..$generator) yield $loopBody" =>
          dealWithLoop(LoopType.forLoop, loopBody, generator = generator)
          (getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        //while(true) and dowhile(true)
        case LabelDef(TermName(name), List(), block@Block(statements, Apply(Ident(TermName(name2)), List())))
          if (name.startsWith("while$") || name.startsWith("doWhile$")) && name2 == name =>
          dealWithLoop(LoopType.trueLoop, block.asInstanceOf[Trees#Tree])
          removeAllInstances() //can set all instances empty since normal code execution will end in while
          // true loop and breakable will take care of the ones which have a break statement
            println("after while true, tracked are "+trackedElements)
          println("after while true, saved instances are "+savedBreakInstances)
          (getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        //while loops
        case q"while ($cond) $loopContents" =>
          dealWithLoop(LoopType.whileLoop, loopContents, cond = cond)
          (getLengthOfTree(line) - 1, None) //-1 because we are processing the current one already
        case q"do $loopContents while ($cond)" =>
          dealWithLoop(LoopType.dowhileLoop, loopContents, cond = cond)
          (0, None)
        //Functions (first is for functions defined in the same scope, second the others)
        case func@Apply(Ident(functionName), args) =>
          var copiedMap = copyMap(trackedElements)
          val returned = dealWithFunction(func, functionName, args)
          updateStateIfNeeded(copiedMap, line)
          (getLengthOfTree(line) - 1, returned) //because we are processing the current one already
        case func@Apply(Select(instanceCalledOn, functionName), args) =>
          
          var copiedMap = copyMap(trackedElements)
          val returned = dealWithFunction(func, functionName, args, instanceCalledOn)
          updateStateIfNeeded(copiedMap, line)
          (getLengthOfTree(line) - 1, returned) //because we are processing the current one already
        //if, try and match statements
        case q"if ($cond) $ifBody else $elseBody" =>
          val returned = dealWithIfElse(cond, ifBody, elseBody)
          println("finished dealing with if/else")
          (getLengthOfTree(line) - 1, returned)
        case q"try $tryBody catch { case ..$cases } finally $finallyBody" =>
          /*_*/ checkTryCatchFinally(tryBody, finallyBody) /*_*/
          (getLengthOfTree(line) - 1, None)
        case q"$expr match { case ..$cases }" =>
          /*_*/ processMatchStatement(expr, cases) /*_*/
          (getLengthOfTree(line) - 1, None)
        //All three next cases are to check for a solitary object name on a line
        case Ident(TermName(elementName)) =>
          
          val objectType = line.symbol.typeSignature.toString
          checkObject(objectType)
          if (line.tpe == null) return (0, None)
          getClosestScopeAliasInfo(objectType, objectType) match {
            case Some(aliasInfo) =>
              val returned = trackedElements(objectType).instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (0, Some(returned))
            case None =>
              if(currentInstance.isEmpty) return (0, None)
              getClosestScopeField(currentInstance.head, elementName) match{
                case Some(field) =>
                  val returned = currentInstance.head.fields(field)
                  (0, Some(returned))
                case None =>
                  (0, None)
              }
          }
        case Select(location, expr) =>
          
          val objectType = line.symbol.typeSignature.toString
          
          val locationType = location.tpe.toString()
          checkObject(objectType)
          //added this to manage case where we have one type element calling a method inside another element but we need to
          //have the current instance be the older element to be able to find the parameters
          var mustPopCurrentInstance = false
          if(currentInstance.isEmpty) return (0, None)
          getClosestScopeAliasInfo(locationType, locationType) match {
            case Some(aliasInfo) =>
              if(currentInstance.head.alias.name != locationType) {
                currentInstance.push(trackedElements(locationType).instances.filter(instance =>
                  instance.containsAliasInfo(aliasInfo._1, aliasInfo._2)).last)
                mustPopCurrentInstance = true
                
              }
            case None =>
          }
          val fields = mutable.Stack((expr.toString, objectType))
          
          getRelevantInstancesAndTheirType(fields) match {
            case Some(instancesAndType) =>
              if(mustPopCurrentInstance) currentInstance.pop()
              (0, Some(instancesAndType._1))
            case None =>
              if(mustPopCurrentInstance) currentInstance.pop()
              (0, None)
          }
        case Block(List(expr), Literal(Constant(()))) =>
          
          val objectType = expr.tpe.toString()
          checkObject(objectType)
          getClosestScopeAliasInfo(objectType, objectType) match {
            case Some(aliasInfo) =>
              val returned = trackedElements(objectType).instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              (0, Some(returned))
            case None =>
              (0, None)
          }
        case q"null" =>
          
          (0, null)
        case _ =>
          (0, None)
      }
    }

    /** Deals with a break statement in the code.
     * Saves the current state of the instances inside savedBreakInstances
     *
     * @param label : Label of the break statement
     */
    private def dealWithBreak(label: String) = {
      breakable {
        for ((labelInMap, typeToInstances) <- savedBreakInstances) {
          if (labelInMap == label) {
            savedBreakInstances(label) += copyMap(trackedElements)
            break()
          }
        }
        savedBreakInstances += label -> ArrayBuffer(copyMap(trackedElements))
      }
    }

    /** Deals with breakable in the code.
     * Checks inside its body.
     * After this, gathers the instances saved under its label during break statements and merges them together.
     * Then removes its label from the savedBreakInstances map
     *
     * @param label : Label of the breakable statement
     * @param body  : Contents of breakable
     */
    def dealWithBreakable(label: Name, body: Seq[Trees#Tree]) = {
      checkInsideObjectBody(body)
      var mapsToMerge = ArrayBuffer[mutable.Map[String, ElementInfo]]()
      for ((labelInMap, typeToInstances) <- savedBreakInstances) {
        if (label.toString() == labelInMap) {
          for(map <- typeToInstances)
            mapsToMerge += map
        }
        savedBreakInstances.remove(label.toString())
      }
      while (mapsToMerge.nonEmpty) {
        trackedElements = mergeMaps(trackedElements, mapsToMerge.last)
        mapsToMerge.remove(mapsToMerge.size- 1)
      }
    }

    /** Processes an assignment to an existing object.
     * First, gets what is returned from the rhs of the assignment operation.
     * Then, checks if the assignee is an alias of an instance.
     * If so, removes that alias from the instances (it is getting replaced by the rhs)
     * Then, add the alias to all the assigned instances (if there are assigned instances).
     *
     * @param fields assignee/lhs  in x = y, x
     * @param assigned     in x = y, y
     */
    def processAssignment(fields:mutable.Stack[(String, String)], assigned: Trees#Tree) = {
      
      val returnedAssigned = checkInsideFunctionBody(assigned)
      
      
      
      getRelevantInstancesAndFieldName(fields) match {
        case Some((instancesToUpdate, fieldName)) =>
          
          currentScope.push(instancesToUpdate.last.alias.name)
          resetClosestScopeField(instancesToUpdate, fieldName)
          for (instance <- instancesToUpdate) {
            returnedAssigned match {
              case Some(instances) =>
                getClosestScopeField(instance, fieldName) match{
                  case Some(field) =>
                    instance.fields(field) = instances
                  case None =>
                }
              case null =>
                getClosestScopeField(instance, fieldName) match{
                  case Some(field) =>
                    instance.fields(field) = Set(Instance(null, Set(State(Undefined, -1)), mutable.Map()))
                  case None =>
                }
              case None =>
                
            }
          }
          currentScope.pop()
        case None =>
      }
    }


    /** Processes a val/var assignee = assigned statement
     * First, gets what is returned from the rhs of the assignment operation.
     * Then, if the assigned is a set of instances, adds the assignee alias to the instances
     * Along the way, gathers the scopes of instances that should be removed and removes them.
     *
     * @param assignee In val/var x = y, this is x. Always comes as a new val or var.
     * @param assigned In val/var x = y, this is y.
     */
    def processNovelAssignment(assignee: String, assigned: Trees#Tree, instancesToAssign:Option[Set[Instance]] = null) = {
      
      
      
      var returnedAssigned:Option[Set[Instance]] = None
      if(instancesToAssign != null)
        returnedAssigned = instancesToAssign
      else
        returnedAssigned = checkInsideFunctionBody(assigned)
      
      
      if(currentInstance.nonEmpty){
        val fieldToAssign = Alias(assignee, currentScope.clone())
        if(currentInstance.head.fields.contains(fieldToAssign)) //check if there is already a field with this exact alias (e.g. from a for loop) and remove it
          currentInstance.head.fields.remove(fieldToAssign)
        returnedAssigned match {
          case Some(instances) =>
            currentInstance.head.fields += (fieldToAssign -> instances)
          case null =>
            currentInstance.head.fields += (fieldToAssign -> Set(Instance(null, Set(State(Undefined, -1)), mutable.Map())))
          case None =>
            
        }
      }
      
    }


    /** Checks if the given expression is of the form x.y() where x is an existing alias and y is a method in
     * its associated protocol.
     *
     * @param expr expression to check
     * @return true if expr is an alias calling a method in its protocol
     */
    def isAliasCallingProtocolMethod(expr: Trees#Tree): Boolean = {
      
      expr match {
        case app@Apply(fun, args) =>
          methodTraverser.traverse(app)
          val methodCallInfos = methodTraverser.methodCallInfos
          //reset the traverser's list to be empty
          methodTraverser.methodCallInfos = ListBuffer[MethodCallInfo]()
          
          for (methodCallInfo <- methodCallInfos) {
            if(methodCallInfo.fields.isEmpty) return false
            
            getRelevantInstancesAndTheirType(methodCallInfo.fields) match{
              case Some(instancesAndType) =>
                if(isProtocolMethod(methodCallInfo.name, instancesAndType._2)) return true
              case None =>
            }
          }
        case _ =>
      }
      false
    }


    /** Given a method name and element type, checks if the method is part of the protocol for the given type.
     *
     * @param methodName  name of the method
     * @param elementType type of the element
     * @return true if the method is part of element's protocol
     */
    def isProtocolMethod(methodName: String, elementType: String): Boolean = {
      if(!trackedElements.contains(elementType) || trackedElements(elementType).methodToIndices == null) return false
      if (trackedElements(elementType).methodToIndices.contains(methodName))
        return true
      false
    }

    /** Processes a match statement.
     * First, checks if the expression being matched (expr) is an alias calling a method from its protocol.
     * If so, goes through the function without updating the alias' state and then goes through the case statement.
     * It will update the state when seeing which return value the function is expected to return
     * If not, checks the expression and then the case statements
     *
     * @param expr  expression being matched
     * @param cases case statements
     */
    def processMatchStatement(expr: Trees#Tree, cases: List[CaseDef]) = {
      if (isAliasCallingProtocolMethod(expr)) {
        expr match {
          case func@Apply(Ident(functionName), args) =>
            val mapBeforeFunction = copyMap(trackedElements)
            dealWithFunction(func, functionName, args)
            processCaseStatements(cases, mapBeforeFunction, expr)
          case func@Apply(Select(instanceCalledOn, functionName), args) =>
            val mapBeforeFunction = copyMap(trackedElements)
            dealWithFunction(func, functionName, args, instanceCalledOn)
            processCaseStatements(cases, mapBeforeFunction, expr)
        }
      }
      else {
        checkInsideFunctionBody(expr)
        processCaseStatements(cases)
      }
    }



    /** Processes case statements
     * Starts by saving the state of the instances before going through any case statements.
     * It then uses those saved instances to go through all the case statements in turn, merging all the resulting
     * instances at the end.
     * For each case statement, if what is matched is a method from a protocol, transitions through
     * the method:returnValue path in the protocol. (The return value being what is matched by the case).
     *
     * @param cases             case statements to process
     * @param mapBeforeFunction Only defined if the expression is an alias calling a method in its protocol; lets
     *                          the update state function check if a function is mutating the state of the alias
     *                          differently than the protocol defines.
     * @param expr              Only defined if the expression is an alias calling a method in its protocol. It is the said expression.
     */
    def processCaseStatements(cases: List[global.CaseDef], mapBeforeFunction: mutable.Map[String, ElementInfo] = null,
                              expr: Trees#Tree = null) = {
      println("before creating before cases, tracked elements are "+trackedElements)
      val beforeCases = copyMap(trackedElements)
      println("before cases is "+beforeCases)
      var mapsToMerge = ArrayBuffer[mutable.Map[String, ElementInfo]]()
      for (singleCase <- cases) {
        println(s"processing case $singleCase")
        println("current instance is "+currentInstance)
        trackedElements = copyMap(beforeCases)
        println("at top of case, tracked elements are "+trackedElements)
        val previousCurInstance= currentInstance.pop()
        val candidateCurInstance = getInstanceWithTypeId(previousCurInstance.alias.name, previousCurInstance.id)
        if(candidateCurInstance == null)
          currentInstance.push(previousCurInstance)
        else
          currentInstance.push(candidateCurInstance)
        println("current instance is now "+currentInstance)
        if (expr != null) { //this is the case where we are dealing with an alias calling a protocolled function being matched
          var returnValue = singleCase.pat.toString()
          if (returnValue.contains(".")) returnValue = returnValue.substring(returnValue.lastIndexOf('.') + 1)
          updateStateIfNeeded(mapBeforeFunction, expr, ":" + returnValue)
          println("in special match, current instance is "+currentInstance)
          val previousCurInstance= currentInstance.pop()
          val candidateCurInstance = getInstanceWithTypeId(previousCurInstance.alias.name, previousCurInstance.id)
          if(candidateCurInstance == null)
            currentInstance.push(previousCurInstance)
          else
            currentInstance.push(candidateCurInstance)
          println("in special match, current instance is now "+currentInstance)
        }
        println(s"before going through case $singleCase, tracked elements are "+trackedElements)
        checkInsideFunctionBody(singleCase.body)
        println("after checking function body, current instance is now "+currentInstance)
        println("at the end of case statement, tracked elements are "+trackedElements)
        mapsToMerge += trackedElements
      }
      while (mapsToMerge.nonEmpty) {
        trackedElements = mergeMaps(trackedElements, mapsToMerge.last)
        mapsToMerge.remove(mapsToMerge.length - 1)
      }
      //update currentInstance
      println("before the end, tracked elements are "+trackedElements)
      println("before the end, current instance is "+currentInstance)
      val previousCurInstance= currentInstance.pop()
      val candidateCurInstance = getInstanceWithTypeId(previousCurInstance.alias.name, previousCurInstance.id)
      if(candidateCurInstance == null)
        currentInstance.push(previousCurInstance)
      else
        currentInstance.push(candidateCurInstance)
      println("at the end of the process case statements fucntion, current instance is "+currentInstance)
      println("at the end, tracked elements are "+trackedElements)
    }

    def assignScope(loopType: LoopType.Value) = {
      loopType match {
        case LoopType.forLoop =>
          currentScope.push("for")
        case LoopType.whileLoop =>
          currentScope.push("while")
        case LoopType.dowhileLoop =>
          currentScope.push("dowhile")
        case LoopType.trueLoop =>
          currentScope.push("true")
      }
    }

    def getAllFieldsOfType(elementType:String):Set[Instance] = {
      var fields:Set[Instance] = Set()
        for(instance <- trackedElements(elementType).instances){
          for(field <- instance.fields.values){
            fields ++= field
          }
        }
      fields
    }

    def getAllFields():Set[Instance] = {
      var fields:Set[Instance] = Set()
      for((elementType, elementInfo) <- trackedElements){
        for(instance <- elementInfo.instances){
          for(field <- instance.fields.values){
            fields ++= field
          }
        }
      }
      fields
    }

    def initialiseInstanceToInterimStates(loopType: LoopType.Value):
                                    mutable.HashMap[String, mutable.HashMap[(Alias, Int), ListBuffer[Set[State]]]] = {
      var instanceToInterimStates: mutable.HashMap[String, mutable.HashMap[(Alias, Int), ListBuffer[Set[State]]]] = mutable.HashMap()
      for ((elementType, elementInfo) <- trackedElements) {
        instanceToInterimStates += (elementType -> mutable.HashMap())
        if (loopType == LoopType.dowhileLoop || loopType == LoopType.trueLoop) {
          //initialise the list to empty since these loops will always execute at least once so we don't want to keep the initial state
          for (instance <- elementInfo.instances) instanceToInterimStates(elementType) += (instance.alias, instance.id) -> ListBuffer()
        } else {
          for (instance <- elementInfo.instances)
            instanceToInterimStates(elementType) += (instance.alias,instance.id) -> ListBuffer(instance.currentStates)
        }
      }
      
      instanceToInterimStates
    }

    /** Deals with any type of loop. The type of loop to be dealt with is defined in the loopType variable.
     * Goes through the loop and at the end saves the state of the instances in instanceToInterimStates.
     * It continues looping over the body of the loop until all the instances' states loop over, at which point
     * we can confirm that the loop is not violating protocol.
     * After this we need to merge the possible states the instances are in (since we don#t know how many times
     * the loop executed) into the main map of instances.
     *
     * @param loopType    enum of which loop type to deal with
     * @param loopContent content of the loop (its body)
     * @param generator   optional generator of a for loop to deal with
     * @param cond        optional condition to deal with (only while loops will have one)
     */
    def dealWithLoop(loopType: LoopType.Value, loopContent: Trees#Tree, generator: Seq[Trees#Tree] = null, cond: Trees#Tree = null) = {
      //initialise instanceToInterimStatesMap
      var instanceToInterimStates = initialiseInstanceToInterimStates(loopType)
      if (loopType == LoopType.forLoop) checkInsideForLoopGenerator(generator) //only need to check this once
      //go through loop
      do {
        println("DIDNT FIND DUPLICATES IN MAP")
        println("map is "+instanceToInterimStates)
        if (loopType == LoopType.whileLoop) checkInsideFunctionBody(cond)
        assignScope(loopType)
        checkInsideFunctionBody(loopContent)
        removeAllFieldsInScope(currentScope)
        currentScope.pop()
        if (loopType == LoopType.dowhileLoop) checkInsideFunctionBody(cond)
        updateMap(instanceToInterimStates)
        
      } while (!duplicatesInAllListsOfMap(instanceToInterimStates))
      if (loopType == LoopType.whileLoop)
        checkInsideFunctionBody(cond) //go through the while loop condition one more time after the body of the loop
      //assigns interim states to the instances
      assignInstancesWithLoopStates(instanceToInterimStates)
    }

    /** Updates the interim states map with the current instances
     *
     * @param instanceToInterimStates map with interim states in a loop
     */
    def updateMap(instanceToInterimStates: mutable.HashMap[String, mutable.HashMap[(Alias, Int), ListBuffer[Set[State]]]]) = {
      for ((elementType, elementInfo) <- trackedElements) {
        if (instanceToInterimStates.contains(elementType)) {
          for (instance <- elementInfo.instances if instanceToInterimStates(elementType).contains(instance.alias, instance.id))
            instanceToInterimStates(elementType)(instance.alias, instance.id) += instance.currentStates
        }
      }

    }

    /** Assigns the interim states acquired while checking a loop to the global set of instances.
     *
     * @param instanceToInterimStates map with interim states in a loop
     */
    def assignInstancesWithLoopStates(instanceToInterimStates:
                                      mutable.HashMap[String, mutable.HashMap[(Alias, Int), ListBuffer[Set[State]]]]) = {
      
      for (elementType <- instanceToInterimStates.keys)
        for ((savedAlias, listOfSetOfStates) <- instanceToInterimStates(elementType))
          for (instance <- (trackedElements(elementType).instances)
               if (instance.alias == savedAlias._1 && instance.id == savedAlias._2 && instance.currentStates != null)) {
            for (setOfStates <- listOfSetOfStates) {
              instance.currentStates = setOfStates ++ instance.currentStates
            }
          }
    }

    /** Checks the code inside a for loop generator
     *
     * @param generator the for loop generator
     */
    def checkInsideForLoopGenerator(generator: Seq[Trees#Tree]) = {
      generator match {
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
        case _ => checkInsideObjectBody(generator)
      }
    }

    /** Handles try-catch in a basic manner, assuming no exceptions.
     * Just goes through try then finally bodies.
     *
     * @param tryBody     Code inside the try block.
     * @param finallyBody Code inside the finally block.
     * @return what is returned from the finally block, if any
     */
    def checkTryCatchFinally(tryBody: Trees#Tree, finallyBody: Trees#Tree): Option[Set[Instance]] = {
      checkInsideFunctionBody(tryBody)
      checkInsideFunctionBody(finallyBody)
    }

    /** Handles an if-else statement.
     * Saves the current state of instances in beforeMap, then
     * goes through both paths and gets new states for the instances.
     * Once this is done it merges the two possible set of instances (afterIfMap and protocolledElements)
     * and protocolledElements then hold all possible states they could be in after going through either path.
     *
     * @param condition Condition inside the if statement
     * @param ifBody    Code inside the if block
     * @param elseBody  Code inside the else block
     * @return What is returned from the if-else statement
     */
    def dealWithIfElse(condition: Trees#Tree, ifBody: Trees#Tree, elseBody: Trees#Tree): Option[Set[Instance]] = {
      checkInsideFunctionBody(condition)
      val beforeMap = copyMap(trackedElements)
      val returnedIfOption = checkInsideFunctionBody(ifBody)
      val afterIfMap = copyMap(trackedElements)
      var returnedIf: Set[Instance] = Set()
      returnedIfOption match {
        case Some(returnedIfValue) =>
          returnedIf = returnedIfValue
        case None =>
      }
      trackedElements = beforeMap
      val returnedElseOption = checkInsideFunctionBody(elseBody)
      var returnedElse: Set[Instance] = Set()
      returnedElseOption match {
        case Some(returnedElseValue) =>
          returnedElse = returnedElseValue
        case None =>
      }
      val returnedIfElse = Option(returnedIf ++ returnedElse)
      trackedElements = mergeMaps(afterIfMap, trackedElements) //at this point, protocolledElements is the map after going through just else
      println("at the end of if/else, tracked are "+trackedElements)
      println("at the end of if/else, current instance is "+currentInstance)
      returnedIfElse
    }

    /** Gets the object which has scope which is closest to the current scope.
     * It goes through the scope, taking it a level down each time, checking if an object with the given type has
     * the scope until a match is found, or the scope is empty (no object could be found).
     *
     * @param objectType type of the object to find
     * @return the object found, if any is found
     */
    def getClosestScopeObject(objectType: String): Option[Element] = {
      val classesAndObjects = ElementTraverser.elements
      
      if (classesAndObjects.isEmpty) return None
      
      val curScope = currentScope.clone()
      while (curScope.nonEmpty) {
        for (element <- classesAndObjects) {
          if (element.elementType == objectType && element.scope == curScope && element.isObject) {
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
     * @param calledOn The object that the method is called on and the one we want to check the constructor for if
     *                 that hasn't been done already
     */
    def checkObjectFunctionCall(calledOn: global.Tree): Unit = {
      if (calledOn == null) return
      val calledOnType = calledOn.tpe.toString()
      for (element <- ElementTraverser.elements
           if (!element.initialised && element.isObject && element.elementType == calledOnType
             && element.scope == getScope(calledOn))) {
        element.initialised = true
        currentScope.push(calledOn.toString())
        checkInsideObjectBody(element.body)
        currentScope.pop()
      }
    }

    def addToBottomOfStack(value: (String, String), fields: mutable.Stack[(String, String)]) =
      {
        val newStack = mutable.Stack(value)
        val tempStack = mutable.Stack[(String, String)]()
        while(fields.nonEmpty){
          tempStack.push(fields.pop())
        }
        while(tempStack.nonEmpty){
          newStack.push(tempStack.pop())
        }
        newStack
      }

    /** Checks if the function given is an assignment function.
     * If it is, checks the assignment and returns true.
     * If not, returns false
     *
     * @param function function to check
     * @return true if function is an assignment
     */
    def isAssignmentFunction(function: global.Apply): Boolean = {
      
      
      methodTraverser.traverse(function)
      val methodCallInfos = methodTraverser.methodCallInfos
      methodTraverser.methodCallInfos = ListBuffer[MethodCallInfo]()
      

      for(methodCallInfo <- methodCallInfos){
        val regex = ".*_\\$eq".r
        regex.findPrefixMatchOf(methodCallInfo.name) match {
          case Some(mat) =>
            val value = mat.toString
            val strippedValue = value.substring(0, value.indexOf("_$eq"))
            
            val fullName = methodCallInfo.name
            var valueType = ""
            if(methodCallInfo.params(0)(0).symbol == null)
              valueType = fullName.substring(fullName.indexOf("_$eq(")+5, fullName.lastIndexOf(")"))
            else
              valueType = methodCallInfo.params(0)(0).symbol.tpe.resultType.toString()
            
            var fields = methodCallInfo.fields
            
            fields = addToBottomOfStack((strippedValue, valueType), fields)
            
            //if(arg1.symbol == null) return false
            processAssignment(fields, methodCallInfo.params(0)(0))
            return true
          case None =>
        }
        false
      }
      false
    }

    def resetClosestScopeField(instancesToUpdate: Set[Instance], field: String) = {
      for(instance <- instancesToUpdate){
        getClosestScopeField(instance, field) match{
          case Some(field) =>
            instance.fields(field) = Set()
          case None =>
        }
      }
    }

    /** Assign the args to the parameters.
     *  Works by equating to actual assignment statements as such:
     *  val parameter = arg
     *  This lets us associate the parameter from the function to the argument given.
     *
     * @param parameters bits in the function definition
     * @param args       bits passed into the function
     * @param calledOn   what the function is called on (in x.y(), x)
     */
    def assignParameters(parameters: ArrayBuffer[(String, String)], args: List[Tree], curInstanceWrongForArgs:Boolean = false) = {
      
      var i = 0
      for (param <- parameters) {
        if (trackedElements.contains(param._2)) {
          if(curInstanceWrongForArgs){
            val curInstToPush = currentInstance.pop()
            val instancesToAssign = processLine(args(i))._2
            currentInstance.push(curInstToPush)
            processNovelAssignment(param._1, args(i), instancesToAssign)
          }
          else {
            processNovelAssignment(param._1, args(i))
          }
        }
        i += 1
      }
    }

    /** Checks all the arguments given
     *
     * @param args arguments given
     */
    def checkArguments(args: List[global.Tree]) = {
      for (arg <- args) {
        checkInsideFunctionBody(arg)
      }
    }

    /** Checks if the cache contains the entry
     *
     * @param cache map with ((elementType, parameterNames, states) -> nextStates) entries
     * @param entry entry of (elementType, parameterNames, states)
     * @return
     */
    def cacheContainsEntry(cache: Map[mutable.Map[String, ElementInfo], mutable.Map[String, ElementInfo]],
                           entry: (mutable.Map[String, ElementInfo])):
    (Boolean) = {
      (cache.contains(entry))
    }

    /** Create a cache entry from the parameters given in the givenToFunctionParams.
     * For each of the parameters of the function, collects the states the instances are in and adds those to the entry.
     *
     * @param givenToFunctionParams
     * @return cache entry as an array of (elementType, parameterNames, states)
     */
    def createCacheEntry(givenToFunctionParams: mutable.HashMap[String, mutable.HashMap[(String, mutable.Stack[String]),
      Set[(String, mutable.Stack[String])]]]): ArrayBuffer[(String, Set[String], Set[State])] = {
      val cacheEntry = ArrayBuffer[(String, Set[String], Set[State])]()
      for (elementType <- givenToFunctionParams.keys) {
        for ((aliasInfo, paramInfos) <- givenToFunctionParams(elementType)) {
          var currentStates = Set[State]()
          for (paramInfo <- paramInfos) {
            val instancesToGetStatesFrom =
              trackedElements(elementType).instances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
            for (instance <- instancesToGetStatesFrom) {
              //if the instance has a protocol
              if(instance != null && instance.currentStates != null) currentStates ++= instance.currentStates
            }
          }
          var paramNames = Set[String]()
          for (paramInfo <- paramInfos) paramNames += paramInfo._1
          cacheEntry.append((elementType, paramNames, currentStates))
        }
      }
      cacheEntry
    }


    /** Get current states of instances in the cache and return them in an appropriate format to be able to add to the
     * cache as nextStates
     *
     * @param cacheEntry entry to update
     * @return array of nextStates to add to the cache entry
     */
    def findNextStates(cacheEntry: ArrayBuffer[(String, Set[String], Set[State])]): ArrayBuffer[Set[State]] = {
      val nextStatesArray = ArrayBuffer[Set[State]]()
      for ((elementType, parameterNames, states) <- cacheEntry) {
        var nextStates = Set[State]()
        for (paramName <- parameterNames) {
          getClosestScopeAliasInfo(paramName, elementType) match {
            case Some(paramInfo) =>
              val instancesWithNextStates =
                trackedElements(elementType).instances.filter(instance => instance.containsAliasInfo(paramInfo._1, paramInfo._2))
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
     * @param firstParameters  ArrayBuffer of parameters as arrays with form [(type, name)]
     * @param secondParameters List of parameters in tree form
     * @return true if the parameters match
     */
    def typesMatch(firstParameters: ArrayBuffer[(String, String)], secondParameters: List[global.Tree]): Boolean = {
      if (!firstParameters.exists(param => param._1.length > 0) && secondParameters.isEmpty) return true //both lists are empty
      
      for ((param, i) <- firstParameters.zipWithIndex) {
        
        if (param._2 != secondParameters(i).tpe.toString() && param._2 != secondParameters(i).tpe.lowerBound.toString)
          return false
      }
      true
    }

    /** Copies instances to be returned if there are instances to return
     *
     * @param returned
     * @return
     */
    def getReturnedFromFunction(returned: Option[Set[Instance]]): Option[Set[Instance]] = {
      returned match{
        case Some(instances) =>
          return Some(copyInstancesWithoutFields((instances)))
        case null =>
          return null
        case None =>
          return None
      }
    }

    def isFunctionOnInstance(calledOn: global.Tree): Boolean = {
      if (calledOn != null) {
        getFields(calledOn) match {
          case Some(fields) =>
            getRelevantInstancesAndTheirType(fields) match {
              case Some(instancesAndType) =>
                
                currentInstance.push(instancesAndType._1.head)
                currentScope.push(instancesAndType._2)
                
                return true
              case None =>
            }
          case None =>
        }
      }
      false
    }

    /** Checks function calls.
     * First it checks if the function is an assignment in which case it just returns to let the assignment
     * be dealt with in the assignment function
     * Then it checks if the function is new x and therefore the code inside a class should be analysed.
     * Then it checks if an object is being called on for the first time and its code should be analysed.
     * Then it goes to analyse inside the function body, renaming the instances to parameter names if needed.
     *
     * @param func         function to deal with
     * @param functionName name of the function
     * @param args         function arguments (passed to it)
     * @param calledOn     optional, what the function is called on (in x.y(), x)
     * @return what is returned from the function, if applicable
     */
    def dealWithFunction(func: global.Apply, functionName: global.Name, args: List[global.Tree], calledOn: Tree = null):
    Option[Set[Instance]] = {
      //region <Checks>

      //check for an assignment function, don't want to check args or do anything else in this case
      if (isAssignmentFunction(func)) return None

      //checks parameters
      checkArguments(args)

      //checks for "new Class()" constructor function
      val isCurrentIsNewAndReturned = checkNewFunction(func, args)
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
             && function.scope == getScope(func, dontCheckSymbolField = true)
             && typesMatch(function.params, args))) {
        
        val shouldPopCurrentInstanceAndScope = isFunctionOnInstance(calledOn)
        //adding calledOn as current element to deal with this.method() calls inside its own function

        currentScope.push(function.name) //push scope so parameters will be scoped inside the function
        
        assignParameters(function.params, args)
        
        val cacheEntry = dealWithCache(function)
        
        if (cacheEntry == null) {
          
          if(shouldPopCurrentInstanceAndScope) {
            currentInstance.pop()
            currentScope.pop()
            
          }
          return getReturnedFromFunction(function.returned)
        }
        //check inside the function body
        currentScope.push("body")
        
        val returned = checkInsideFunctionBody(function.body)
        
        
        //todo figuring out what is returned PUT THIS INTO ITS OWN FUNCTION
        returned match {
          case Some(setOfInstances) =>
            val scopeClone = currentScope.clone()
            var instancesReturned = setOfInstances//delete internal variables
            scopeClone.pop()
            instancesReturned = instancesReturned//need to delete the parameters too
            function.returned = Some(copyInstancesWithoutFields(instancesReturned))
          case null =>
            function.returned = null
          case None =>
            function.returned = None
        }
        //remove aliases inside the body of the function since they can't be used anymore
        removeAllFieldsInScope(currentScope)
        currentScope.pop()
        //update cache
        function.stateCache += cacheEntry -> copyMap(trackedElements)
        
        //delete aliases defined in the function
        removeAllFieldsInScope(currentScope)
        currentScope.pop()
        if(shouldPopCurrentInstanceAndScope) {
          currentInstance.pop()
          currentScope.pop()
          
        }
        return getReturnedFromFunction(function.returned)
      }
      None
    }

    def parameterNamesPointingAtInstances(parameters: ArrayBuffer[(String, String)], instancesPointedAt: Set[Instance]):Set[String] = {
      var names = Set[String]()
      if(currentInstance.isEmpty) return names
      for(parameter <- parameters){
        getClosestScopeField(currentInstance.head, parameter._1) match{
          case Some(field) =>
            if(currentInstance.head.fields(field) == instancesPointedAt)
              names += field.name
          case None =>
        }
      }
      names
    }

    /** Check cache for hit with the current instances.
     * If there is a hit with next states then change the states and return null, indicating that the
     * dealWithFunction function should return.
     * If there is a hit with null then we are in a recursive function and should thus skip the next call and
     * return null
     * If there is no hit, create a new cache entry and return it
     *
     * @param function function to check the cache of
     * @return either a new entry for the cache or null if we should leave the function as a result of a cache hit
     */
    def dealWithCache(function: Function): (mutable.Map[String, ElementInfo]) = {
      val cacheEntry = copyMap(trackedElements)
      val cacheHit = cacheContainsEntry(function.stateCache, cacheEntry)
      //mutate state if possible and skip recursive call if needed
      if (cacheHit && function.stateCache(trackedElements) == null) {
        removeTopLevelAliasesInScope(currentScope)
        currentScope.pop()
        return null
      }
      if (cacheHit) {
        trackedElements = copyMap(function.stateCache(cacheEntry))
        removeTopLevelAliasesInScope(currentScope)
        println("cache entry is ",function.stateCache)
        println("CACHE HIT, SKIPPING")
        currentScope.pop()
        return null
      }

      //if it doesn't hit, put in a null entry
      function.stateCache += cacheEntry -> null
      cacheEntry
    }

    /** Checks if the object given has been seen before. If not, executes the code inside it.
     *  If the string passed is not an object type, does nothing
     *
     * @param objectType type of object to check (only need this since there will always only be one instance of an object
     */
    def checkObject(objectType: String) = {
      getClosestScopeObject(objectType) match {
        case Some(obj) =>
          if (!obj.initialised) {
            obj.initialised = true
            currentScope.push(objectType)
            getClosestScopeAliasInfo(objectType, objectType) match{
              case Some(aliasInfo) => currentInstance.push(trackedElements(objectType).instances.filter(instance =>
                                        instance.containsAliasInfo(aliasInfo._1, aliasInfo._2)).last)
              case None =>
            }
            checkInsideObjectBody(obj.body)
            if(currentInstance.nonEmpty) currentInstance.pop()
            currentScope.pop()
          }
        case _ =>
      }
    }


    /** Checks for a new x function and executes the code within the class if found. Renames instances to
     * constructor parameter names if needed.
     *
     * @param function function to check
     * @param args     possible constructor arguments for the function
     * @return 1:true if the
     */
    def checkNewFunction(function: global.Apply, args: List[Tree]): (Boolean, Boolean, Set[Instance]) = {
      var isCurrentType = false
      var returned: Set[Instance] = Set()
      function match {
        case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          parents match {
            case List(item) =>
              
              //note here getScope is called on the symbol owner of the function since we want to skip the class name in the scope
              val IsCurrentTypeAndReturned =
                checkInsideClass(getScope(function.symbol.owner), function.tpe.toString(), args)
              isCurrentType = IsCurrentTypeAndReturned._1
              returned = IsCurrentTypeAndReturned._2
            case List(Apply(className, arg2)) =>
              
              val IsCurrentTypeAndReturned = checkInsideClass(getScope(className), className.tpe.toString(), args)
              isCurrentType = IsCurrentTypeAndReturned._1
              returned = IsCurrentTypeAndReturned._2
            case _ =>
          }
          return (isCurrentType, true, returned)
        case _ =>
      }
      (false, false, returned)
    }

    def getSimpleClassName(elementType: String): String = {
      var tempName = elementType
      if(elementType.endsWith(".type"))
        tempName = elementType.stripSuffix(".type")
      if (tempName.contains("."))
        tempName.substring(tempName.lastIndexOf(".") + 1)
      else tempName
    }

    /** Checks inside a class body
     *
     * @param scope       scope of the class
     * @param elementType type of the class
     * @param args        constructor arguments
     * @return
     */
    def checkInsideClass(scope: mutable.Stack[String], elementType: String, args: List[Tree]): (Boolean, Set[Instance]) = {
      
      var returned: Set[Instance] = Set()

      //find correct class to go through
      for (element <- ElementTraverser.elements
           if !element.isObject && element.elementType == elementType && element.scope == scope) {
        //create new instance of this class and update scope and currentinstance
        val newInstanceName = elementType
        currentScope.push(elementType)
        if (trackedElements.contains(elementType)) {
          if(trackedElements(elementType).states != null) {
            
            var newClassInstance = Instance(Alias(newInstanceName, currentScope.clone()),
              Set(trackedElements(elementType).states(0)), mutable.Map(), id = element.id)
            trackedElements(elementType).instances += newClassInstance
            currentInstance.push(newClassInstance)
          } else{
            
            var newClassInstance = Instance(Alias(newInstanceName, currentScope.clone()), null, mutable.Map(), id = element.id)
            trackedElements(elementType).instances += newClassInstance
            currentInstance.push(newClassInstance)
          }
        }
        assignParameters(element.params, args, true)
        
        checkInsideObjectBody(element.body)
        if (trackedElements.contains(elementType)) {
          for (instance <- trackedElements(elementType).instances) {
            if (instance.containsAliasInfo(newInstanceName, currentScope)) {
              returned = Set(instance)
              
            }
          }
        }
        //todo (check) don't want to remove created instances anymore => removeTopLevelAliasesInScope(currentScope)
        element.id += 1
        currentScope.pop()
        if(currentInstance.nonEmpty) currentInstance.pop()
        
      }
      (trackedElements.contains(elementType), returned)
    }

    /** For the parameter list given, checks if they match any of our defined instances. If so, renames the instance
     * to the parameter name. Keeps memory of the renaming in a hashmap of parameter name to instance name so these
     * can easily be renamed after the function exits.
     *
     * @return
     */
    def createMap(parameters: ArrayBuffer[(String, String)]):
    mutable.HashMap[String, mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]] = {
      
      val givenToFunctionParam = new mutable.HashMap[String, mutable.HashMap[(String, mutable.Stack[String]), Set[(String, mutable.Stack[String])]]]()
      for (elementType <- trackedElements.keys)
        givenToFunctionParam += (elementType -> mutable.HashMap())
      var argCounter = 0
      val paramScope = currentScope.clone()
      for (param <- parameters) {
        val argName = param._1
        getClosestScopeAliasInfo(argName, param._2) match {
          case Some(aliasInfo) =>
            val paramName = parameters(argCounter)._1
            givenToFunctionParam(param._2).get(aliasInfo._1, aliasInfo._2) match {
              case Some(setOfParams) =>
                val updatedSet = setOfParams ++ Set((paramName, paramScope))
                givenToFunctionParam(param._2) += (aliasInfo._1, aliasInfo._2) -> updatedSet
              case None =>
                givenToFunctionParam(param._2) += (aliasInfo._1, aliasInfo._2) -> Set((paramName, paramScope))
            }
          case None =>
        }
        argCounter += 1
      }
      givenToFunctionParam
    }

    def checkForInconsistentStateMutation(instancesToUpdate: Set[Instance], originalInstances: Set[Instance],
                                          aliasInfo: (String, mutable.Stack[String]), aliasType: String,
                                          methodName: String, line: Trees#Tree) = {
      for (instance <- originalInstances) {
        updateInstance(instance, methodName, line, trackedElements(aliasType), aliasType)
      }
      if (!(instancesToUpdate == originalInstances)) {
        var expectedStates: Set[State] = Set()
        for (instance <- originalInstances) {
          expectedStates ++= instance.currentStates
        }
        var actualStates: Set[State] = Set()
        for (instance <- instancesToUpdate) {
          actualStates ++= instance.currentStates
        }

        throw new inconsistentStateMutation(methodName, aliasInfo._1,
          line.pos.source.toString(), line.pos.line, expectedStates, actualStates)
      }
    }

    /** For a given line of code, checks if it is a method on an instance with protocol and if so updates its state
     *
     * @param line
     */
    def updateStateIfNeeded(mapBeforeFunction: mutable.Map[String, ElementInfo], line: Trees#Tree, returnValue: String = "") = {
      /*_*/ methodTraverser.traverse(line.asInstanceOf[Tree]) /*_*/
      val methodCallInfos = methodTraverser.methodCallInfos
      methodTraverser.methodCallInfos = ListBuffer[MethodCallInfo]()
      println("method call infos are "+methodCallInfos)
      //println("found method info "+methodCallInfos)
      for (methodCallInfo <- methodCallInfos) {
        var methodName = methodCallInfo.name + returnValue
        if (methodName.contains(".") && methodName.contains("(")) {
          methodName = methodName.substring(0, methodName.indexOf("(") + 1) + methodName.substring(methodName.lastIndexOf(".") + 1)
        }
        breakable {
          val fields = methodCallInfo.fields
          println("fields are "+fields)
          getRelevantInstancesAndTheirType(fields) match{
            case Some((instancesToUpdate, instancesType)) =>
              if(!trackedElements.contains(instancesType) || trackedElements(instancesType).states == null) break()
              val currentElementInfo = trackedElements(instancesType)
              if (!currentElementInfo.methodToIndices.contains(methodName) && !currentElementInfo.returnValueToIndice.contains(methodName)) {
                if (!methodName.contains(":") || !currentElementInfo.methodToIndices.contains(methodName.substring(0, methodName.indexOf(":")))) {
                  break()
                }
              }
              for (instance <- instancesToUpdate)
                updateInstance(instance, methodName, line, currentElementInfo, instancesType)
            case None =>
          }
        }
      }
    }


    /** Given an instance to look through and the name of a field,
     *  goes through the instance looking for the field with the given name
     *  which has scope closest to the current scope.
     *
     * @param instance
     * @param fieldName
     * @return
     */
    def getClosestScopeField(instance: Instance, fieldName: String): Option[Alias] = {
      val curScope = currentScope.clone()
      while (curScope.nonEmpty) {
        for (field <- instance.fields.keys) {
          if (field.name == fieldName && field.scope == curScope)
            return Some(field)
        }
        curScope.pop()
      }
      None
    }

    def getRelevantInstancesAndFieldName(fields:mutable.Stack[(String, String)]): Option[(Set[Instance], String)] ={
      
      
      if(fields.isEmpty || currentInstance.isEmpty) return None
      var relevantInstances = Set(currentInstance.head)
      var instancesType = currentInstance.head.alias.name
      
      while(fields.size > 1){
        val (fieldName, fieldType) = fields.pop()
        var curRelevantInstances:Set[Instance] = Set()
        currentScope.push(instancesType)
        for(instance <- relevantInstances){
          getClosestScopeField(instance, fieldName) match{
            case Some(field) =>
              curRelevantInstances ++= instance.fields(field)
            case None =>
              
          }
        }
        currentScope.pop()
        if(curRelevantInstances.nonEmpty) {
          relevantInstances = curRelevantInstances
          instancesType = fieldType
        }
      }
      val fieldName = fields.pop()._1
      for(instance <- relevantInstances){
        
        if (!instance.containsFieldName(fieldName))
          return None
      }
      Some(relevantInstances, fieldName)
    }

    def getRelevantInstancesAndTheirType(fields:mutable.Stack[(String, String)]): Option[(Set[Instance], String)] ={
      if(currentInstance.isEmpty) return None
      println("currentInstance in relevant instance is "+currentInstance)
      var relevantInstances = Set(currentInstance.head)
      var instancesType:String = currentInstance.head.alias.name
      while(fields.nonEmpty){
        val (fieldName, fieldType) = fields.pop()
        var curRelevantInstances:Set[Instance] = Set()
        currentScope.push(instancesType)
        for(instance <- relevantInstances){
          getClosestScopeField(instance, fieldName) match{
            case Some(field) =>
              if(instance.fields.contains(field))
                curRelevantInstances ++= instance.fields(field)
              else curRelevantInstances -= instance
            case None =>
          }
        }
        currentScope.pop()
        if(curRelevantInstances.nonEmpty)
          relevantInstances = curRelevantInstances
        instancesType = fieldType
      }
      if(relevantInstances.isEmpty) return None
      Some(relevantInstances, instancesType)
    }

    def getFieldNamesPointingAtInstance(instanceToFind:Instance): Set[String] ={
      var fieldNames = Set[String]()
      for((elementType, elementInfo) <- trackedElements)
        for(instance <- elementInfo.instances) {
          breakable {
            for ((field, instances) <- instance.fields) {
              if (instances.isEmpty) break()
              if (instances.last.alias != null && instances.contains(instanceToFind))
                fieldNames += field.name
            }
          }
        }
      fieldNames
    }

    def getPossibleMethods(elementType: String, states: Set[State]): Set[ReturnValue] = {
      
      if(trackedElements(elementType).stateToAvailableMethods ==null) return Set()
      
      var possibleMethods = trackedElements(elementType).stateToAvailableMethods(states.last)
      
      for (state <- states) {
        possibleMethods = possibleMethods.intersect(trackedElements(elementType).stateToAvailableMethods(state))
      }
      possibleMethods
    }

    def updateInstance(instance: Instance, methodName: String, line: Trees#Tree, currentElementInfo: ElementInfo, elementType: String) = {
      println(s"Updating instance $instance of type $elementType, using method $methodName.")
      var newSetOfStates: Set[State] = Set()
      for (state <- instance.currentStates) {
        if (state.name == Undefined)
          throw new usedUninitialisedException(methodName, sortSet(getFieldNamesPointingAtInstance(instance)), elementType, line.pos.line)
        var indexSet: Set[Int] = Set()
        if (currentElementInfo.methodToIndices.contains(methodName))
          indexSet = currentElementInfo.methodToIndices(methodName)
        else if (currentElementInfo.returnValueToIndice.contains(methodName))
          indexSet = Set(currentElementInfo.returnValueToIndice(methodName))
        else {
          val rawMethodName = methodName.substring(0, methodName.indexOf(":"))
          indexSet = Set(currentElementInfo.methodToIndices(rawMethodName).min)
        }
        var newStates: Set[State] = Set[State]()
        newStates += currentElementInfo.transitions(state.index)(indexSet.min)
        if (indexSet.size > 1 && currentElementInfo.transitions(state.index)(indexSet.min).name == Undefined)
          newStates = for (x <- indexSet - indexSet.min) yield currentElementInfo.transitions(state.index)(x)
        for (state <- newStates if state.name == Undefined) {
          val possibleNextMethods = getPossibleMethods(elementType, instance.currentStates)
          throw new protocolViolatedException(sortSet(getFieldNamesPointingAtInstance(instance)), elementType,
            sortSet(instance.currentStates), methodName, line.pos.source.toString(), line.pos.line,
            formatMethods(sortSet(possibleNextMethods)))
        }
        newSetOfStates = newSetOfStates ++ newStates
      }
      for(instanceToChange <- trackedElements(elementType).instances){
        if(instanceToChange == instance)
          instanceToChange.currentStates = newSetOfStates
      }
      instance.currentStates = newSetOfStates
      //println("after updating, instances are")
      //printInstances()
    }

    def formatMethods(methods: SortedSet[ReturnValue]): String = {
      var formattedMethods = ""
      for (method <- methods) {
        if (method.valueName != Any)
          formattedMethods += method.parentMethod.name + ":" + method.valueName + " "
        else formattedMethods += method.parentMethod.name + " "
      }
      if (formattedMethods == "")
        formattedMethods = "No methods are available in this state."
      formattedMethods
    }

    //region<Traversers>

    def getSimpleFields(qualifier: Tree): mutable.Stack[(String, String)]={
      
      var qualifierTree = qualifier
      
      val fields = mutable.Stack[(String, String)]() //name and type of each field
      while(qualifierTree.children.nonEmpty){
        fields.push((qualifierTree.symbol.name.toString(), qualifierTree.symbol.tpe.toString()))
        qualifierTree = qualifierTree.children.last
      }
      fields.push((qualifierTree.symbol.name.toString(), qualifierTree.symbol.tpe.toString()))
      
      fields
    }

    /** Gets the fields of an expression.
     *  The fields are stored inside a stack with (name, type) and the topmost element is the leftmost element of the expression (expr)
     *
     * @param expr
     * @return the fields as a Stack of string tuples
     */
    def getFields(qualifier:Tree): Option[mutable.Stack[(String, String)]]={
      if(currentInstance.isEmpty) return None
      
      var qualifierTree = qualifier
      
      val fields = mutable.Stack[(String, String)]() //name and type of each field
      while(qualifierTree.children.nonEmpty){
        fields.push((qualifierTree.symbol.name.toString(), qualifierTree.symbol.tpe.toString()))
        qualifierTree = qualifierTree.children.last
      }
      
      val owner = qualifierTree.symbol
      //case where the qualifier did not include the owner name (as happens when we are in the main() method and not in an object
      if(owner == null) {
        return Some(fields)
      }
      
      getClosestScopeAliasInfo(owner.tpe.toString(), owner.tpe.toString()) match{
        case Some(aliasInfo) =>
          //case where we have a single object name to deal with
          if(fields.isEmpty && currentInstance.head.alias.name != owner.tpe.toString()) {
            
            fields.push((qualifierTree.symbol.name.toString(), qualifierTree.symbol.tpe.toString()))
          }
        case None =>
          
          //case where the qualifier did not include the owner name (as happens when we are in the main() method and not in an object
          if(currentInstance.head.alias.name != owner.tpe.toString())
            fields.push((qualifierTree.symbol.name.toString(), qualifierTree.symbol.tpe.toString()))
      }
      
      Some(fields)
    }

    case class MethodCallInfo(name: String, fields: mutable.Stack[(String,String)], params:List[List[Tree]]){
      def simpleName(): String ={
        name.substring(0,name.indexOf("("))
      }
      def parameters(): ListBuffer[(String, String)]={
        getParameters(params)
      }
    }

    /** Traverses a tree and collects (methodName, aliasName) from method application statements
     *
     */
    object methodTraverser extends Traverser {
      var methodCallInfos = ListBuffer[MethodCallInfo]()
      override def traverse(tree: Tree): Unit = {
        tree match {
          case app@Apply(fun, args) =>
            app match {
              case q"$expr(...$exprss)" =>
                expr match {
                  case select@Select(qualifier, name) =>
                    getFields(qualifier) match {
                      case Some(fields) =>
                        /*_*/
                        methodCallInfos += MethodCallInfo(name + getParameterTypesFromTree(exprss), fields, exprss)
                        /*_*/
                        
                      case None =>
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


    /** Traverses a tree and collects elements found inside "elements" */
    object ElementTraverser extends Traverser {
      var elements = ListBuffer[Element]()

      override def traverse(tree: Tree): Unit = {
        tree match {
          case obj@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
            elements += Element(obj.symbol.tpe.toString(), obj.symbol, ArrayBuffer(), body, getScope(obj), isObject = true)
            addElementProtocolIfItExists(body, obj.symbol.tpe.toString(), tree, objectName = obj.symbol.tpe.toString())
            super.traverse(obj)
          case clas@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
            val parameters = /*_*/ getParameters(paramss) /*_*/
            elements += Element(clas.symbol.tpe.toString(), clas.symbol, parameters, body, getScope(clas))
            addElementProtocolIfItExists(body, clas.symbol.tpe.toString(), tree)
            super.traverse(clas)
          case _ =>
            super.traverse(tree)
        }
      }
    }

    /** Gathers function informations in the code inside "functions" */
    object functionTraverser extends Traverser {
      var functions = ListBuffer[Function]()

      override def traverse(tree: Tree): Unit = {
        tree match {
          case func@q"$mods def $funcName[..$tparams](...$paramss): $returnType = $body" =>
            val parameters = /*_*/ getParameters(paramss) /*_*/
            if (funcName.toString() != "<init>") { //ignore constructors here as they will be dealt with better later
              functions +=
                Function(funcName.toString(), func.symbol.owner, parameters, returnType, body, getScope(func),
                  Map[mutable.Map[String, ElementInfo], mutable.Map[String, ElementInfo]](), returned = None)
            }
            /*_*/ super.traverse(body) /*_*/
          case _ =>
            super.traverse(tree)
        }
      }
    }


    //endregion

    def isTypestateAnnotation(annotations: List[global.AnnotationInfo]): (Boolean, String) = {
      for (annotation@AnnotationInfo(arg1, arg2, arg3) <- annotations) {
        getFilenameFromTypestateAnnotation(annotation) match {
          case Some(protocolName) =>
            return (true, protocolName)
          case None =>
            return (false, "")
        }
      }
      (false, "")
    }

    /** Checks whether the object or class is following its protocol in the code.
     * It first checks if the element has a typestate annotation, then runs the protocol and collects the information
     * from it.
     * Then it checks the methods in the protocol are a subset of those defined in the element.
     * Then it checks the protocol is followed
     *
     * @param body        The code to check
     * @param elementType Name of the element
     * @param tree        The entire code
     * @param objectName  If this is an object, then the name of the object, otherwise: null
     * @return
     */
    def addElementProtocolIfItExists(body: Seq[Trees#Tree], elementType: String,
                                     tree: Tree, objectName: String = null) = {
      
      if (isTypestateAnnotation(tree.symbol.annotations)._1) {
        val protocolName = isTypestateAnnotation(tree.symbol.annotations)._2
        //retrieve the serialized data
        val (stateMachine, states, returnValuesArray) = getDataFromProtocol(protocolName)
        val methodToIndices = createMethodToIndicesMap(returnValuesArray)
        val returnValueStringToIndice = createReturnValueStringToIndiceMap(returnValuesArray)
        val returnValueToIndice = createReturnValueToIndiceMap(returnValuesArray)
        val stateToAvailableMethods = createStateToAvailableMethodsMap(stateMachine, returnValueToIndice, states) //could use in error message to tell users what methods are available in the current state
        checkProtocolMethodsSubsetElementMethods(methodToIndices.keys.toArray, body, elementType, protocolName)
        val currentElementInfo = ElementInfo(stateMachine, states, methodToIndices, returnValueStringToIndice,
          stateToAvailableMethods, Set[Instance]())
        if (objectName != null)
          currentElementInfo.objectName = objectName
        trackedElements += elementType -> currentElementInfo
      }
      else{
        trackedElements += elementType -> ElementInfo(null, null, null, null, null, Set[Instance](), objectName)
      }
      //
    }


    /** Contains the state of instances at a break statement
     * Structured as a map of breakable label -> elementType -> instances
     *
     */
    var savedBreakInstances: mutable.Map[String, ArrayBuffer[mutable.Map[String, ElementInfo]]] =
      mutable.Map[String, ArrayBuffer[mutable.Map[String, ElementInfo]]]()

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
        for (symbol <- obj.symbol.owner.ownerChain.reverse) {
          objectScope.push(symbol.tpe.toString())
        }
      }
      objectScope
    }

    /** Gets the scope of a symbol by looking through its owner chain
     *
     * @param symbol
     * @return mutable stack representing the scope of the given symbol
     */
    def getScope(symbol: Symbol): mutable.Stack[String] = {
      val objectScope = mutable.Stack[String]()
      for (symbol <- symbol.owner.ownerChain.reverse)
        objectScope.push(symbol.tpe.toString())
      objectScope
    }

    /** Gets a parameter string as formatted in a function definition from a tree of them */
    def getParameterTypesFromTree(params: List[List[Tree]]): String = {
      params match {
        case List(List()) => "()"
        case List(List(value)) => keepOnlyName(value.tpe.toString()).mkString("(", "", ")")
        case List(values) =>
          var parameters: ArrayBuffer[String] = ArrayBuffer()
          for (elem <- values) {
            parameters += keepOnlyName(elem.tpe.toString)
          }
          parameters.mkString("(", ",", ")")
        case _ => ""
      }
    }

    def getParameters(params: List[List[Tree]]): ListBuffer[(String, String)] ={
      var parameters = ListBuffer[(String, String)]()
      params match {
        case List(List()) =>
        case List(List(value)) =>
          parameters.append((value.toString, keepOnlyName(value.tpe.toString())))
        case List(values) =>
          for(elem <- values) {
            parameters.append((elem.toString, keepOnlyName(elem.tpe.toString)))
          }
        case _ =>
      }
      parameters
    }

    /** Checks that methods in the protocol are a subset of those in the body of the element
     *
     * @param rawProtocolMethods : names of the methods in the protocol
     * @param elementBody        : body of the class or object being checked
     * @param filename           : name of the file the user code is in to generate a useful error message
     */
    def checkProtocolMethodsSubsetElementMethods(rawProtocolMethods: Array[String], elementBody: Seq[Trees#Tree],
                                                 elementType: String, filename: String): Unit = {
      val elementMethods = getMethodNames(elementBody)
      val protocolMethods =
        for (method <- rawProtocolMethods) yield stripReturnValue(method.replaceAll("\\s", ""))
      if (!(protocolMethods.toSet subsetOf elementMethods)) throw new badlyDefinedProtocolException(
        s"Methods ${protocolMethods.toSet} defined in $filename are not a subset of methods " +
          s"$elementMethods defined in class $elementType. Methods ${protocolMethods.toSet.diff(elementMethods)} are defined in " +
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
            val parameters = /*_*/ getParameterTypes(paramss) /*_*/
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
    def getParameterTypes(params: List[List[ValDef]]): String = {
      params match {
        case List(List()) => ""
        case List(List(value)) =>
          var valueName = value.tpt.toString()
          if (valueName.contains('.'))
            valueName = valueName.substring(valueName.lastIndexOf('.') + 1)
          valueName
        case List(values) =>
          var parameters: ArrayBuffer[String] = ArrayBuffer()
          for (elem <- values) {
            var valueName = elem.tpt.toString
            if (valueName.contains('.'))
              valueName = valueName.substring(valueName.lastIndexOf('.') + 1)
            parameters += valueName
          }
          parameters.mkString(",")
        case _ => ""
      }
    }


    /** Gets parameters from a tree as their name and type in a string array */
    def getParameters(params: List[List[ValDef]]): ArrayBuffer[(String, String)] = {
      params match {
        case List(List()) => ArrayBuffer(("", null))
        case List(List(value)) =>
          ArrayBuffer((value.name.toString(), value.tpt.toString()))
        case List(values) =>
          var parameters: ArrayBuffer[(String, String)] = ArrayBuffer()
          for (elem <- values) {
            parameters += ((elem.name.toString(), elem.symbol.tpe.toString()))
          }
          parameters
        case _ => ArrayBuffer()
      }
    }


    /** Checks to see if there are duplicates in all the lists of a map(Instance -> list) */
    def duplicatesInAllListsOfMap(maps: mutable.HashMap[String, mutable.HashMap[(Alias, Int), ListBuffer[Set[State]]]]): Boolean = {
      for (map <- maps.values) {
        for ((instance, list) <- map) for ((instance, list) <- map if list.nonEmpty && list.diff(list.distinct).isEmpty) {
          println("RETURNING FALSE")
          return false
        }
        println("RETURNING TRUE")
      }
      true
    }

    /** Makes a deepcopy of the trackedElements datastructure */
    def copyMap(map: mutable.Map[String, ElementInfo]): mutable.Map[String, ElementInfo] = {
      var copiedMap = mutable.Map[String, ElementInfo]()
      for ((elementType, elementInfo) <- map) {
        copiedMap += (elementType -> ElementInfo(elementInfo.transitions, elementInfo.states,
          elementInfo.methodToIndices, elementInfo.returnValueToIndice, elementInfo.stateToAvailableMethods,
          copyInstancesWithoutFields(elementInfo.instances)))
      }
      copiedMap = addFields(copiedMap, map)
      copiedMap
    }

    def addFields(newMap: mutable.Map[String, ElementInfo], oldMap: mutable.Map[String, ElementInfo]): mutable.Map[String, ElementInfo] ={
      for((elementType, elementInfo) <- oldMap){
        for(instance <- elementInfo.instances){
          var newMapInstance = newMap(elementType).instances.filter(newInstance => newInstance == instance).last
          for((field, instancesPointedTo) <- instance.fields){
            breakable {
                //get instances pointed to (in first map) from the newMap and same for second map pointed instances
                var instancesToPointTo = Set[Instance]()
                if(instancesPointedTo.isEmpty || instancesPointedTo.last.alias == null) break()
                var fieldType = instancesPointedTo.last.alias.name
                for (instance <- newMap(fieldType).instances) {
                  if (instancesPointedTo.contains(instance)) {
                    instancesToPointTo += instance
                  }
                }
                newMapInstance.fields += (field -> instancesToPointTo)
              }
          }
        }
      }
      newMap
    }

    def removeAllAliasesInScope(scope: mutable.Stack[String]): Unit ={
      removeAllFieldsInScope(scope)
      removeTopLevelAliasesInScope(scope)
    }

    def removeTopLevelAliasesInScope(scope: mutable.Stack[String]) = {
      breakable {
        if (scope == null) break()
        for ((elementType, elementInfo) <- trackedElements) {
          for (instance <- elementInfo.instances) {
              if (instance.alias.scope == scope)
                elementInfo.instances -= instance
          }
          trackedElements(elementType).instances = cleanInstances(elementInfo.instances)
        }
      }
    }

    def removeScopedFromInstances(instances: Set[Instance], scope: mutable.Stack[String]): Set[Instance]={
      
      
      for(instance <- instances){
        for((fieldAlias, fieldInstances) <- instance.fields){
          
          if (fieldAlias.scope == scope) {
            
            instance.fields -= fieldAlias
          } else if(fieldInstances != null && fieldInstances != Set())
            removeScopedFromInstances(fieldInstances, scope)
        }
      }
      instances
    }

    def removeAllFieldsInScope(scope: mutable.Stack[String]) = {
      breakable {
        if (scope == null) break()
        for ((elementType, elementInfo) <- trackedElements) {
          for (instance <- trackedElements(elementType).instances) {
            if (instance.alias.scope == scope)
              trackedElements(elementType).instances -= instance
            else {
              for (field <- instance.fields.keys) {
                if (field.scope == scope) instance.fields -= field
              }
            }
          }
        }
      }
    }

  }

}








