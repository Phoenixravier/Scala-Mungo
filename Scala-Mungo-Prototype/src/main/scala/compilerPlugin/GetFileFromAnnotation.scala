package compilerPlugin

import java.nio.file.{Files, Paths}

import ProtocolDSL.{ReturnValue, State}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}
import scala.util.control.Breaks._
import scala.tools.nsc._

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
                      returnType:Trees#Tree, body: Trees#Tree, scope: mutable.Stack[String],
                      stateCash:Map[(String, Set[State]), Set[State]]) { //might replace string with elementInfo for more precision (on build)
    override def toString(): String = {
      s"name: $name parameters: $params return type: $returnType scope: $scope body: $body"
    }
  }

  val runsAfter: List[String] = List[String]("refchecks")
  val phaseName: String = "compilerPlugin.GetFileFromAnnotation.this.name"
  def newPhase(_prev: Phase) = new GetFileFromAnnotationPhase(_prev)
    /** Phase which is ran by the plugin */
    class GetFileFromAnnotationPhase(prev: Phase) extends StdPhase(prev) {
      var compilationUnit:CompilationUnit=_
      var currentElementInfo: ElementInfo=_
      override def name: String = "compilerPlugin.GetFileFromAnnotation.this.name"

      /** Entry point of the plugin. Goes through the code collecting object, class and function bodies, then checks
       * the code for protocol violations
       * @param unit: contains tree of the code in body
       */
      def apply(unit: CompilationUnit): Unit = {
        compilationUnit = unit
        //find all the classes, objects and functions in the code so we can jump to them later
        functionTraverser.traverse(unit.body)
        classAndObjectTraverser.traverse(unit.body)

        //println(functionTraverser.functions)
        checkCode()
      }

      /** Finds objects and classes with protocols and goes through the code checking that the protocols are followed
       *
       */
      def checkCode(): Unit ={
        var setOfClassesWithProtocols: Set[String] = Set()
        for (tree@q"$mods class $className[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body) {
          checkElement(body, className.toString(),  getScope(tree), tree) match{
            case Some(className) => setOfClassesWithProtocols += className
            case None =>
          }
        }
        for(tree@q"$mods object $name extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body){
          val objectName = mutable.Stack[String]()
          objectName.push(name.toString())
          checkElement(body, name.toString(), objectName, tree, true) match{
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
       * @param body
       * @param name
       * @param tree
       * @param isObject
       * @return
       */
      def checkElement(body:Seq[Trees#Tree], name:String, scope:mutable.Stack[String], tree:Tree, isObject:Boolean=false): Option[String] ={
        val annotations = tree.symbol.annotations
        for(annotation@AnnotationInfo(arg1,arg2, arg3) <- annotations){
          getFilenameFromTypestateAnnotation(annotation) match{
            case Some(filename) => { //a correct Typestate annotation is being used
              //execute the DSL in the protocol file and serialize the data into a file
              Util.executeFile(filename)
              //retrieve the serialized data
              if(!Files.exists(Paths.get("protocolClasses\\EncodedData.ser")))
                throw new badlyDefinedProtocolException(s"The protocol at $filename could not be processed, " +
                  s"check you have an end statement at the end of the protocol and that the name of the file is " +
                  s"the same as the name of the protocol and that the path given for the protocol is correct")
              val (transitions, states, returnValuesArray) = Util.getDataFromFile("protocolClasses\\EncodedData.ser")
              checkProtocolMethodsSubsetClassMethods(returnValuesArray, body, name, filename)
              val methodToIndices = Util.createMethodToIndicesMap(returnValuesArray)
              currentElementInfo = ElementInfo(name, scope, transitions, states, methodToIndices, isObject)
              println("checking class "+currentElementInfo.name)
              checkElementIsUsedCorrectly()
              Some(name)
            }
            case None => None
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
      def checkElementIsUsedCorrectly(): Unit ={
        for(line@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" <- compilationUnit.body){
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
                  if (getParameters(paramss) == "Array[String]") {
                    checkObject(tname.toString())
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
       * @param code
       */
      def checkInsideObjectBody(code:Seq[Trees#Tree], givenInstances:Set[Instance]=Set()): Set[Instance] = {
        var instances = for (instance <- givenInstances) yield instance
        if(currentElementInfo.isObject) {
          instances +=Instance(currentElementInfo.name, Set(), Set(currentElementInfo.states(0)))
          for(instance <- instances if instance.aliases.isEmpty)
            instance.aliases += Alias(currentElementInfo.name, Util.currentScope.clone)
        }
        for (line <- code)
            instances = checkInsideFunctionBody(line, instances)._1
        println("\nInstances:")
        instances.foreach(println)
        instances
      }


      /** Checks the code inside the body of a function for protocol violations.
       * Goes line by line and skips lines if needed (for example when at a definition or a loop).
       *
       * @param code
       * @param givenInstances
       * @return
       */
      def checkInsideFunctionBody(code:Trees#Tree, givenInstances:Set[Instance]=Set()): (Set[Instance], Option[Any]) ={
        var instances = for (instance <- givenInstances) yield instance
        if(currentElementInfo.isObject) { //TODO deal with the fact this happens every time this function is called
          instances +=Instance(currentElementInfo.name, Set(), Set(currentElementInfo.states(0)))
          for(instance <- instances if instance.aliases.isEmpty)
            instance.aliases += Alias(currentElementInfo.name, Util.currentScope.clone)
        }
        var returned: Option[Any] = None
        var nbOfLinesToSkip = 0
        for (line <- code) {
          breakable {
            if(nbOfLinesToSkip>0){
              nbOfLinesToSkip-=1
              break
            }
            val newInstanceAndNbLinesToSkipAndReturned = processLine(line, instances)
            instances = newInstanceAndNbLinesToSkipAndReturned._1
            nbOfLinesToSkip = newInstanceAndNbLinesToSkipAndReturned._2
            if(newInstanceAndNbLinesToSkipAndReturned._3.isDefined)
              returned = newInstanceAndNbLinesToSkipAndReturned._3
            println(s"after processing line $line, returned is $returned")
          }
        }
        println("\nInstances:")
        instances.foreach(println)
        (instances, returned)
      }

      def processAssignment(assignee: String, assigned: Trees#Tree, instances: Set[Instance]): Set[Instance] = {
        println("in process assignment")
        println("assignee is "+assignee)
        println("assigned is "+assigned)
        val newInstancesAndReturned = checkInsideFunctionBody(assigned, instances)
        var newInstances = newInstancesAndReturned._1
        val returnedAssigned = newInstancesAndReturned._2
        println(s"returned assignee is $assignee and returned assigned is $returnedAssigned")
        getClosestScopeAliasInfo(assignee, newInstances) match{
          case Some(assigneeAliasInfo) =>
            val assigneeInstancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(assigneeAliasInfo._1, assigneeAliasInfo._2))
            returnedAssigned match{
              case Some(assignedAliasInfoOrName) =>
                assignedAliasInfoOrName match {
                  case _: (String, mutable.Stack[String]) =>
                    val assignedAliasInfo = assignedAliasInfoOrName.asInstanceOf[(String, mutable.Stack[String])]
                    val assignedInstancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(assignedAliasInfo._1, assignedAliasInfo._2))
                    removeAliases(assigneeInstancesToUpdate, assigneeAliasInfo._1)
                    for(instance <- assignedInstancesToUpdate)
                      instance.aliases += Alias(assigneeAliasInfo._1, assigneeAliasInfo._2)
                  case _:((String, mutable.Stack[String]), Set[State]) =>
                    val aliasInfoAndStates = assignedAliasInfoOrName.asInstanceOf[((String, mutable.Stack[String]), Set[State])]
                    val aliasInfo = aliasInfoAndStates._1.asInstanceOf[(String, mutable.Stack[String])]
                    val states = aliasInfoAndStates._2
                    removeAliases(assigneeInstancesToUpdate, assigneeAliasInfo._1)
                    newInstances += Instance(currentElementInfo.name, Set(Alias(aliasInfo._1, aliasInfo._2)), states)
                  case _ =>
                    throw new Exception("Went somewhere unexpected")
                }
              case None =>
                removeAliases(assigneeInstancesToUpdate, assigneeAliasInfo._1)
            }
          case None =>
        }
        newInstances
      }

      /** Processes a val assignee = assigned statement
       * Checks if assigned is an existing alias and if so adds assignee to its list of aliases
       *
       * @param assignee Always comes as a new val or var
       * @param assigned
       * @param instances
       * @return
       */
      def processNovelAssignment(assignee: TermName, assigned: Trees#Tree, instances: Set[Instance]): Set[Instance] = {
        println("in process novel assignment")
        println("assignee is "+assignee)
        println("assigned is "+assigned)
        var (newInstances, returnedAssigned) = checkInsideFunctionBody(assigned, instances)
        println("returnedAssigned is "+returnedAssigned)
        returnedAssigned match{
          case Some(aliasesOrName) =>
            newInstances = dealWithAssignedToNovel(assignee, newInstances, aliasesOrName)
          case _ =>
            //if nothing is being assigned then do nothing
        }
        newInstances
      }

      private def dealWithAssignedToNovel(assignee: global.TermName, instances: Set[Instance], aliasInfoOrName: Any): Set[Instance] = {
        println("in the function")
        println("aliasInfoOrName is "+aliasInfoOrName)
        println("class is "+aliasInfoOrName.getClass)
        var newInstances = for(instance <- instances) yield instance
        aliasInfoOrName match {
          case _: (Any, Any) =>
            val aliasTuple = aliasInfoOrName.asInstanceOf[(Any, Any)]
            aliasTuple._1 match{
              //function returns case
              case _: Option[(String, mutable.Stack[String])] =>
                println("found extra states")
                val aliasInfoAndStates = aliasInfoOrName.asInstanceOf[(Option[(String, mutable.Stack[String])], Set[State])]
                aliasInfoAndStates._1 match{
                  case Some(aliasInfoUntyped) =>
                    val aliasInfo = aliasInfoUntyped.asInstanceOf[(String, mutable.Stack[String])]
                    val states = aliasInfoAndStates._2
                    println("got something returned from function")
                    //case where an instance was created inside a function and returned
                    //add new instance
                    newInstances += Instance(currentElementInfo.name, Set(Alias(assignee.toString(), Util.currentScope.clone())), states)
                  case None =>
                }
              //normal case
              case _: String =>
                println("matched tuple")
                val aliasInfo = aliasInfoOrName.asInstanceOf[(String, mutable.Stack[String])]
                //check if alias already exists
                getClosestScopeAliasInfo(aliasInfo._1, newInstances) match {
                  case Some(aliasInfo) =>
                    //if already exists then add to list of aliases
                    var instancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
                    for(instance <- instancesToUpdate)
                      instance.aliases += Alias(assignee.toString(), Util.currentScope.clone())
                    println("instances after dealing with this are "+newInstances)
                  case None =>
                    //otherwise add as new instance
                    newInstances += Instance(currentElementInfo.name, Set(), Set(State("init", 0)))
                    Util.addInMissingAlias(newInstances, assignee.toString)
                }
            }
          //if else case
          case _: Array[Option[Any]] =>
            var ifElseResult = aliasInfoOrName.asInstanceOf[Array[Option[Any]]]
            for (option <- ifElseResult) {
              option match {
                case Some(ifElseAliasInfosOrName) =>
                  println(s"putting $assignee and $ifElseAliasInfosOrName through the function again")
                  newInstances = dealWithAssignedToNovel(assignee, instances, ifElseAliasInfosOrName)
                case None =>
              }
            }
          case _ =>
            println("matched nothing")
        }
        newInstances
      }



      /** Checks a line and returns possibly updated instances.
       *  Has different cases for different types of line
       *
       * @param line line of code to analyse
       * @param instances instances to update
       * @return
       */
      def processLine(line:Trees#Tree, instances: Set[Instance]): (Set[Instance], Int, Option[Any]) ={
        println(s"checking line $line at line number "+line.pos.line)
        println(s"instances before checking line are $instances")
        Util.printBanner()
        println(showRaw(line))
        line match {
          //definitions to skip over (object, class, function)
          case q"$modifs object $tname extends { ..$earlydefins } with ..$pparents { $sself => ..$body }" =>
            (instances, Util.getLengthOfTree(line)-1, None)
          case q"$mod class $pname[..$tpara] $actMods(...$para) extends { ..$defs } with ..$prnts { $self => ..$sts }" =>
            (instances, Util.getLengthOfTree(line)-1, None)
          case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr"=>
            (instances, Util.getLengthOfTree(line)-1, None)
          //new instance declarations (val and var)
          case q"$mods val $tname: $tpt = new $classNm(...$exprss)" =>
            println("found a new val statement")
            var newInstances = processNewInstance(tname, classNm, instances)
            (newInstances,0, None)
          case q"$mods var $tname: $tpt = new $classNm(...$exprss)" =>
            var newInstances = processNewInstance(tname, classNm, instances)
            (newInstances,0, None)
          //assignment
          case q"val $assignee = $newValue" =>
            println("matched equals with val")
            val newInstances = processNovelAssignment(assignee, newValue, instances)
            (newInstances, Util.getLengthOfTree(line)-1, None)
          case q"var $assignee = $newValue" =>
            println("matched equals with var")
            val newInstances = processNovelAssignment(assignee, newValue, instances)
            (newInstances, Util.getLengthOfTree(line)-1, None)
          case q"$assignee = $newValue" =>
            println("in assignment "+line)
            println("assignee1 is "+assignee)
            val newInstances = processAssignment(assignee.toString, newValue, instances)
            (newInstances, Util.getLengthOfTree(line)-1, None)
          //loops
          case q"for (..$enums) $expr" =>
            println("matched for loop in process line")
            val newInstances = dealWithForLoop(enums, instances, expr)
            (newInstances, Util.getLengthOfTree(line)-1, None) //-1 because we are processing the current one already
          case q"for (..$enums) yield $expr" =>
            println("matched for yield")
            val newInstances = dealWithForLoop(enums, instances, expr)
            (newInstances, Util.getLengthOfTree(line)-1, None) //-1 because we are processing the current one already
            //while(true) and dowhile(true)
          case LabelDef(TermName(name), List(), block @ Block(statements, Apply(Ident(TermName(name2)), List())))
            if (name.startsWith("while$") || name.startsWith("doWhile$")) && name2 == name =>{
            val newInstances = dealWithLoopContents(instances, block.asInstanceOf[Trees#Tree])
            (newInstances, Util.getLengthOfTree(line)-1, None) //-1 because we are processing the current one already
          }
          case q"while ($cond) $loopContents" =>{
            val newInstances = dealWithWhileLoop(cond, instances, loopContents)
            (newInstances, Util.getLengthOfTree(line)-1, None) //-1 because we are processing the current one already
          }
          case q"do $loopContents while ($cond)" =>{
            val newInstances = dealWithDoWhileLoop(cond, instances, loopContents)
            (newInstances, 0, None)
          }
          //Functions (first is for functions defined in the same scope, second the others)
          case func@Apply(Ident(functionName), args) =>{
            val (newInstances, returned) = dealWithFunction(func, functionName, args, instances)
            val updatedInstances = updateStateIfNeeded(newInstances, line)
            (updatedInstances, 0, returned) //because we are processing the current one already
          }
          case func@Apply(Select(instanceCalledOn, functionName), args) =>{
            println("called on is spawned on line "+line)
            val (newInstances, returned) = dealWithFunction(func, functionName, args, instances, instanceCalledOn)
            val updatedInstances = updateStateIfNeeded(newInstances, line)
            (updatedInstances, 0, returned) //because we are processing the current one already
          }
          case q"if ($cond) $ifBody else $elseBody" =>
            println("dealing with if else "+line)
            println("if body is "+ifBody)
            println("else body is "+elseBody)
            val (newInstances, returned) = dealWithIfElse(cond, ifBody, elseBody, instances)
            println("returned from ifelse is "+returned)
            (newInstances,Util.getLengthOfTree(line)-1, Some(returned))
          case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
            (instances, 0, Some(line))
          case q"try $tryBody catch { case ..$cases } finally $finallyBody" =>
            val newInstances = checkTryCatchFinally(tryBody, cases, finallyBody, instances)
            (newInstances, Util.getLengthOfTree(line)-1, None)
          case q"$expr match { case ..$cases }" =>
            println("before function")
            val newInstances = processMatchStatement(expr, cases, instances)
            println("Found a match statement")
            println("expr is "+expr)
            println("cases are "+cases)
            (newInstances, Util.getLengthOfTree(line)-1, None)
          //All three next cases are to check for solitary object name on a line
          case Ident(TermName(objectName)) =>
            println("matched raw ident")
            checkObject(objectName, instances)
            val returned = getClosestScopeAliasInfo(objectName, instances)
            (instances,0, returned)
          case Select(location, expr) =>
            println("matched raw select")
            var exprString = expr.toString()
            if(exprString.lastIndexOf(".") != -1)
              exprString = exprString.substring(exprString.lastIndexOf(".")+1)
            checkObject(exprString, instances)
            val returned = getClosestScopeAliasInfo(exprString.trim, instances)
            (instances,0, returned)
          case Block(List(expr), Literal(Constant(()))) =>
            println("matched raw block")
            var exprString = expr.toString()
            if(exprString.lastIndexOf(".") != -1)
              exprString = exprString.substring(exprString.lastIndexOf(".")+1)
            checkObject(exprString, instances)
            val returned = getClosestScopeAliasInfo(exprString.trim, instances)
            (instances,0, returned)
          //default case
          case q"$name" =>
            println("matched name")
            (instances,0, None)
          case _ =>
            println("nothing matched this line")
            (instances,0, None)
        }
      }

      def processCaseStatements(cases: List[global.CaseDef], instances: Set[Instance], instancesToMerge: Set[Instance]): Set[Instance] = {
        //copy instances into newInstances properly like in if/else code
        println("in recursive process case function")
        var caseInstances:Set[Instance] = Set()
        for (instance <- instances) caseInstances += Instance(instance.className, instance.aliases, instance.currentStates)
        //this needs to actually process what is inside the case statement rather than the entire statement
        val newInstances = checkInsideFunctionBody(cases.head.body, caseInstances)._1
        if(cases.tail.nonEmpty)
          return processCaseStatements(cases.tail, instances, newInstances)
        else
          return mergeInstanceStates(newInstances, instancesToMerge)
      }

      def processMatchStatement(expr: Trees#Tree, cases: List[CaseDef], instances: Set[Instance]): Set[Instance] = {
        println("in process match stmt function")
        var newInstances = for(instance <- instances) yield instance
        //first go through the expr
        val newInstancesAndReturned = checkInsideFunctionBody(expr, instances) //have this as two lines because just ._1 wasn't working here oddly
        newInstances = newInstancesAndReturned._1
        newInstances = processCaseStatements(cases, newInstances, newInstances)
        newInstances
      }

      def dealWithWhileLoop(cond: Trees#Tree, instances: Set[Instance], loopContent: Trees#Tree) = {
        //initialisations
        var newInstances = for(instance <- instances) yield instance
        var instanceToInterimStates: mutable.HashMap[Instance, ListBuffer[Set[State]]] = mutable.HashMap()
        for(instance <- newInstances) instanceToInterimStates += instance -> ListBuffer(instance.currentStates)
        //loop
        do{
          //go through condition of the while
          newInstances = checkInsideFunctionBody(cond, newInstances)._1
          //go through loop body
          newInstances = checkInsideFunctionBody(loopContent, newInstances)._1
          for(instance <- newInstances if instanceToInterimStates.contains(instance))
            instanceToInterimStates(instance) += instance.currentStates
        } while(!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
        //go through condition of the while one last time before exiting as that is how the program will execute
        newInstances = checkInsideFunctionBody(cond, newInstances)._1
        //assigns interim states to the instances
        for(instance <- newInstances if instanceToInterimStates.contains(instance))
          for(setOfStates <- instanceToInterimStates(instance))
            instance.currentStates = instance.currentStates ++ setOfStates
        newInstances
      }

      def dealWithDoWhileLoop(cond: Trees#Tree, instances: Set[Instance], loopContent: Trees#Tree) = {
        //initialisations
        var newInstances = for(instance <- instances) yield instance
        var instanceToInterimStates: mutable.HashMap[Instance, ListBuffer[Set[State]]] = mutable.HashMap()
        for(instance <- newInstances) instanceToInterimStates += instance -> ListBuffer()
        //loop
        do{
          //go through loop body
          newInstances = checkInsideFunctionBody(loopContent, newInstances)._1
          //go through condition of the while
          newInstances = checkInsideFunctionBody(cond, newInstances)._1
          for(instance <- newInstances if instanceToInterimStates.contains(instance))
            instanceToInterimStates(instance) += instance.currentStates
        } while(!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
        //assigns interim states to the instances
        for(instance <- newInstances if instanceToInterimStates.contains(instance)){
          for(setOfStates <- instanceToInterimStates(instance))
            instance.currentStates = instance.currentStates ++ setOfStates
        }
        newInstances
      }

      def checkInsideForLoopGenerator(enums: Seq[Trees#Tree], instances: Set[Instance]): Set[Instance] = {
        var newInstances = for(instance <- instances) yield instance
        enums match{
          case List(fq"$pat <- $beginning until $end") =>
            for(begin <- beginning.children)
              newInstances = checkInsideFunctionBody(begin, newInstances)._1
            newInstances = checkInsideFunctionBody(end, newInstances)._1
          case List(fq"$pat <- $beginning to $end") =>
            for(begin <- beginning.children)
              newInstances = checkInsideFunctionBody(begin, newInstances)._1
            newInstances = checkInsideFunctionBody(end, newInstances)._1
          case List(fq"$pat <- $gen") =>
            println("matched pure generator")
            newInstances = checkInsideFunctionBody(gen, newInstances)._1
          case _ => newInstances = checkInsideObjectBody(enums, newInstances)
        }
        newInstances
      }

      def dealWithForLoop(enums: Seq[Trees#Tree], instances: Set[Instance], loopContent: Trees#Tree): Set[Instance] = {
        //initialisations
        var newInstances = for(instance <- instances) yield instance
        var instanceToInterimStates: mutable.HashMap[Instance, ListBuffer[Set[State]]] = mutable.HashMap()
        for(instance <- newInstances) instanceToInterimStates += instance -> ListBuffer(instance.currentStates)
        println("after instantiation, map is "+instanceToInterimStates)
        //loop
        do{
          //go through condition of the for
          Util.printBanner()
          println("before checking for loop generator "+enums)
          newInstances = checkInsideForLoopGenerator(enums, newInstances)
          println("after checking for loop generator")
          //go through loop body
          newInstances = checkInsideFunctionBody(loopContent, newInstances)._1
          //update map with new states of the instances
          println("new instances are "+newInstances)
          for(instance <- newInstances if instanceToInterimStates.contains(instance))
            instanceToInterimStates(instance) += instance.currentStates
          println("right before check for duplicates, map is "+instanceToInterimStates)
        } while(!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
        //assigns interim states to the instances
        for(instance <- newInstances if instanceToInterimStates.contains(instance)){
          for(setOfStates <- instanceToInterimStates(instance))
            instance.currentStates = instance.currentStates ++ setOfStates
        }
        newInstances
      }

      /** Handles try-catch in a basic manner, assuming no exceptions.
       * Just goes through try then finally bodies.
       *
       * @param tryBody
       * @param cases
       * @param finallyBody
       * @param instances
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
       * @param ifBody
       * @param elseBody
       * @param instances
       * @return
       */
      def dealWithIfElse(condition: Trees#Tree, ifBody: Trees#Tree, elseBody: Trees#Tree, instances:Set[Instance]):(Set[Instance], Array[Option[Any]]) = {
        var newInstances = for(instance <- instances) yield instance
        newInstances = checkInsideFunctionBody(condition, newInstances)._1

        var ifInstances:Set[Instance] = Set()
        for (instance <- newInstances) ifInstances += Instance(instance.className, instance.aliases, instance.currentStates)

        var elseInstances:Set[Instance] = Set()
        for (instance <- newInstances) elseInstances += Instance(instance.className, instance.aliases, instance.currentStates)
        println("going into if body "+ifBody)
        val (newIfInstances, returnedIf) = checkInsideFunctionBody(ifBody, ifInstances)
        println("going into else body "+elseBody)
        val (newElseInstances, returnedElse) = checkInsideFunctionBody(elseBody, elseInstances)
        println(s"returned from if is $returnedIf, returned from else is $returnedElse")
        (mergeInstanceStates(newIfInstances, newElseInstances), Array(returnedIf, returnedElse))
      }

      /** Handles code which creates a new instance of a class. Checks if the new instance is of the class we are
       * currently handling and then checks if the instance is already defined in the list of instances.
       * If so, it replaces the old instance with a new one. If not it adds a new instance to the list.
       *
       * @param instanceTree
       * @param classTree
       * @param instances
       * @return
       */
      def processNewInstance(instanceTree:TermName, classTree:Tree, instances:Set[Instance]):Set[Instance]= {
        println("processing new instance")
        val className = currentElementInfo.name
        val states = currentElementInfo.states
        var newInstances = for (instance <- instances) yield instance
        println(s"tree name is ${classTree.toString} and class name is $className")
        if (classTree.toString.contains(className)) {
          newInstances = removeAliases(newInstances, instanceTree.toString())
          newInstances += Instance(className, Set(), Set(states(0)))
          for(instance <- newInstances if instance.aliases.isEmpty)
            instance.aliases += Alias(instanceTree.toString(), Util.currentScope.clone)
        }
        newInstances
      }


      /** For two sets of instances, if and instance is present in both of them, merges the different states
       * associated with it into one instance. Copies over the remaining instances which are only present once.
       *
       * @param firstInstances
       * @param secondInstances
       * @return
       */
      def mergeInstanceStates(firstInstances:Set[Instance], secondInstances:Set[Instance]): Set[Instance] ={
        var mergedInstances:Set[Instance] = Set()
        for(firstInstance <- firstInstances){
          for(alias <- firstInstance.aliases) {
            secondInstances.find(instance => instance.aliases.contains(alias)) match {
              case Some(instance) =>
                mergedInstances += Instance(firstInstance.className, firstInstance.aliases ++ instance.aliases,
                  firstInstance.currentStates ++ instance.currentStates)
              case None => mergedInstances += firstInstance
            }
          }
        }
        for(secondInstance <- secondInstances) if(!firstInstances.contains(secondInstance)){
          mergedInstances += secondInstance
        }
        mergedInstances
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
      def getClosestScopeAliasInfo(name:String, instances:Set[Instance]): Option[(String, mutable.Stack[String])] = {
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
       *
       * @param objectName
       * @return
       */
      def getClosestScopeObject(objectName: String): Option[ClassOrObject] ={
        val classesAndObjects = classAndObjectTraverser.classesAndObjects
        if(classesAndObjects.isEmpty) return None
        val curScope = Util.currentScope.clone()
        while(curScope.nonEmpty){
          for(element <- classesAndObjects){
            if(element.name == objectName && element.scope == curScope){
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
       * @param calledOn
       * @param instances
       */
      def checkObjectFunctionCall(calledOn: global.Tree, instances:Set[Instance]): Unit = {
        if(calledOn == null) return
        var calledOnString = calledOn.toString()
        if(calledOnString.lastIndexOf(".") != -1)
          calledOnString = calledOn.toString.substring(calledOn.toString.lastIndexOf(".")+1)
        for (element <- classAndObjectTraverser.classesAndObjects
             if(!element.initialised && element.isObject && element.name == calledOnString
               && element.scope == getScope(calledOn))) {
          element.initialised = true
          Util.currentScope.push(calledOnString)
          checkInsideObjectBody(element.body, instances)
          Util.currentScope.pop()
        }
      }

      def replaceAliases(paramNameScopeToAlias: mutable.HashMap[(String, mutable.Stack[String]), (String, mutable.Stack[String])], instances: Set[Instance]): Set[Instance] = {
        for((parameter, alias) <- paramNameScopeToAlias) {
          for(instance <- instances if instance.aliases.contains(Alias(parameter._1, parameter._2))) {
            instance.aliases -= Alias(parameter._1, parameter._2)
            instance.aliases += Alias(alias._1, alias._2)
          }
        }
        instances
      }

      /** Checks function calls.
       * First it checks if the function is an assignment in which case it just returns to let the assignment
       *  be dealt with in the assignment function
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
      def dealWithFunction(funcCall: global.Apply, functionName: global.Name, args: List[global.Tree], instances:Set[Instance], calledOn:Tree=null):(Set[Instance], Option[Any]) = {
        println("found function call "+funcCall)
        //check for an assignment function
        funcCall match{
          case Apply(Select(arg1, TermName(functionName)), List(arg3)) =>
            val regex = ".*_\\$eq".r
            regex.findPrefixMatchOf(functionName) match{
              case Some(mat) =>
                println("value is "+mat)
                println("called on is "+calledOn)
                println("found assignment, going into function")
                val value = mat.toString
                processAssignment(value.substring(0, value.length-4), arg3, instances)
                return (instances, None)
              case None =>
            }
          case _ =>
        }
        println("checking parameters "+args)
        var newInstances = for(instance <- instances) yield instance
        //checks parameters
        for(arg <- args) checkInsideFunctionBody(arg, instances)
        println(s"after checking parameters for function call $funcCall with name $functionName")
        newInstances = checkNewFunction(funcCall, instances, args)
        checkObjectFunctionCall(calledOn, instances)
        //finding function definition
        val functionScope = getScope(funcCall, true)
        for (function <- functionTraverser.functions){
          if(function.name == functionName.toString() && function.scope == functionScope) {
            println("matched functions, found body "+function.body)

            //handling parameters on entry
            val paramNameScopeToAlias = handleFunctionParameters(args, function.params,function.name, instances)
            //checking inside the function body
            Util.currentScope.push(functionName.toString())
            val newInstancesAndReturned = checkInsideFunctionBody(function.body, instances)
            newInstances = newInstancesAndReturned._1
            //figuring out what is returned
            var returned = newInstancesAndReturned._2
            returned match{
              case Some(alias) =>
                alias match{
                  case _:(String, mutable.Stack[String]) =>
                    val aliasInfo = alias.asInstanceOf[(String, mutable.Stack[String])]
                    println(s"trying to match ${function.returnType.toString()} and ${currentElementInfo.name}")
                    returned =
                      if(function.returnType.toString().contains(currentElementInfo.name)) {
                        println("matched types")
                        //instance created outside the function
                        if(paramNameScopeToAlias.contains((aliasInfo._1, aliasInfo._2))) {
                          Some(aliasInfo._1)
                        }
                        //instance created inside the function
                        else {
                          println("HERE instances are "+newInstances)
                          newInstances.find(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2)) match{
                            case Some(instance) =>
                              Some(newInstancesAndReturned._2, instance.currentStates)
                            case None =>
                              newInstancesAndReturned._2
                          }
                        }
                      }
                      else newInstancesAndReturned._2
                  case _ =>
                }

              case None =>
            }
            Util.currentScope.pop()
            //renaming parameters on exit
            newInstances = replaceAliases(paramNameScopeToAlias, instances)
            println(s"the deal with function returns $returned")
            println(s"instances are $newInstances")
            return (newInstances, returned)
          }
        }
        println("instances at the end of deal with function are "+instances)
        (instances, None)
      }

      /** Checks if the object given has been seen before. If not, executes the code inside it.
       *
       * @param objectName
       * @param instances
       * @return
       */
      def checkObject(objectName: String, instances:Set[Instance]=Set()) = {
        println(s"inside check Object with obj name $objectName")
        getClosestScopeObject(objectName) match{
          case Some(obj) =>
            obj.initialised = true
            Util.currentScope.push(objectName)
            checkInsideObjectBody(obj.body, instances)
            Util.currentScope.pop()
          case _ =>
        }
      }

      /** Checks for a new x function and executes the code within the class if found. Renames instances to
       * constructor parameter names if needed.
       *
       * @param funcCall
       * @param instances
       * @param args
       */
      def checkNewFunction(funcCall: global.Apply, instances:Set[Instance], args: List[global.Tree]): Set[Instance] ={
        var newInstances = for(instance <- instances) yield instance
        funcCall match{
          case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
            parents match {
              case List(Apply(elementName, arg2)) =>
                var elementNameString = elementName.toString()
                if(elementNameString.lastIndexOf(".") != -1)
                  elementNameString = elementNameString.substring(elementNameString.lastIndexOf(".")+1)
                for (element <- classAndObjectTraverser.classesAndObjects
                     if(!element.isObject && element.name == elementNameString
                       && element.scope == getScope(elementName))) {
                  val paramNameScopeToAlias = handleFunctionParameters(args, element.params, element.name, instances)
                  Util.currentScope.push(element.name)
                  checkInsideObjectBody(element.body, instances)
                  Util.currentScope.pop()
                  newInstances = replaceAliases(paramNameScopeToAlias, instances)
                }
              case _ =>
            }
          case _ =>
        }
        newInstances
      }

      /** For the parameter list given, checks if they match any of our defined instances. If so, renames the instance
       * to the parameter name. Keeps memory of the renaming in a hashmap of parameter name to instance name so these
       * can easily be renamed after the function exits.
       *
       * @param args
       * @param instances
       * @return
       */
      def handleFunctionParameters(args:List[global.Tree], parameters:ArrayBuffer[Array[String]], functionName:String, instances:Set[Instance]): mutable.HashMap[(String, mutable.Stack[String]), (String, mutable.Stack[String])] ={
        var paramNameToAliasName = new mutable.HashMap[(String, mutable.Stack[String]), (String, mutable.Stack[String])]
        var argCounter = 0
        for(arg <- args){
          var argString = arg.toString()
          if(argString.contains(".")) argString = argString.substring(argString.lastIndexOf(".")+1)
          getClosestScopeAliasInfo(argString, instances) match{
            case Some(aliasInfo) =>
              println(s"found aliases with info $aliasInfo in parameters")
              val paramScope = Util.currentScope.clone().push(functionName)
              val paramName = parameters(argCounter)(0)
              paramNameToAliasName += (paramName, paramScope) -> (aliasInfo._1, aliasInfo._2)
              val newInstances = instances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
              for(instance <- newInstances)
                instance.updateAlias(Alias(aliasInfo._1, aliasInfo._2), Alias(paramName, paramScope))
            case None =>
          }
          argCounter += 1
        }
        paramNameToAliasName
      }

      /** Handles any for or while loop.
       * It goes through the contents of the for loop and checks what the states of all the instances are at the end.
       * It stores theses states in a list (one for each instance) and checks to see if all instances have looped
       * (i.e. they have twice the same set of states in their list). If so it gets out of the for loop. It then
       * gives all instances all the states they went through while looping since we don't know how many times the loop
       * will iterate between 0 and infinity times. This assumes that users cannot write infinte loops into their
       * protocols, otherwise this would never terminate.
       *
       *
       * @param instances
       * @param loopContent
       * @return
       */
      def dealWithLoopContents(instances:Set[Instance], loopContent:Trees#Tree): Set[Instance] ={
        var newInstances = for(instance <- instances) yield instance
        var instanceToInterimStates: mutable.HashMap[Instance, ListBuffer[Set[State]]] = mutable.HashMap()
        for(instance <- newInstances) instanceToInterimStates += instance -> ListBuffer(instance.currentStates)
        do{
          for(instance <- newInstances if instanceToInterimStates.contains(instance))
            instanceToInterimStates(instance) += instance.currentStates
          for(line <- loopContent) {
            newInstances = processLine(line, newInstances)._1
            for(updatedInstance <- newInstances if instanceToInterimStates.contains(updatedInstance)) {
              instanceToInterimStates(updatedInstance)(instanceToInterimStates(updatedInstance).length - 1) = updatedInstance.currentStates
            }
          }
        } while(!Util.duplicatesInAllListsOfMap(instanceToInterimStates))
        for(instance <- newInstances if instanceToInterimStates.contains(instance)){
          for(setOfStates <- instanceToInterimStates(instance))
            instance.currentStates = instance.currentStates ++ setOfStates
        }
        newInstances
      }

      /** For a given line of code, checks if it is a method on an instance with protocol and if so updates its state
       *
       * @param instances
       * @param line
       */
      def updateStateIfNeeded(instances: Set[compilerPlugin.Instance], line:Trees#Tree): Set[Instance] ={
        println("inside update state for line "+line)
        val methodToStateIndices = currentElementInfo.methodToIndices
        val elementName = currentElementInfo.name
        line match{
          case app@Apply(fun, args) => {
            methodTraverser.traverse(app)
          }
          case _ =>
        }
        val methodCallInfos = methodTraverser.methodCallInfos
        for(methodCallInfo <- methodCallInfos){
          val methodName = methodCallInfo(0)
          val aliasName = methodCallInfo(1)
          println("instances inside update are "+instances)
          println("alias name to find within those is "+aliasName)
          //use find to get all instances which have this alias in them
          getClosestScopeAliasInfo(aliasName, instances) match{
            case Some(aliasInfo) =>
              val aliasName = aliasInfo._1
              val aliasScope = aliasInfo._2
              instances.find(instance => instance.containsAliasInfo(aliasName, aliasScope)) match {
                case Some(instance) =>
                  breakable {
                    var newSetOfStates: Set[State] = Set()
                    for (state <- instance.currentStates) {
                      if (state.name == Util.Unknown) break
                      if (methodToStateIndices.contains(methodName)) {
                        println("found method name " + methodName)
                        val indexSet = methodToStateIndices(methodName)
                        println("index set is " + indexSet)
                        var newStates: Set[State] = Set[State]()
                        newStates += currentElementInfo.transitions(state.index)(indexSet.min)
                        if (indexSet.size > 1 && currentElementInfo.transitions(state.index)(indexSet.min).name == Util.Undefined)
                          newStates = for (x <- indexSet - indexSet.min) yield currentElementInfo.transitions(state.index)(x)
                        println("new states are " + newStates)
                        for (state <- newStates if state.name == Util.Undefined) {
                          throw new protocolViolatedException(Util.sortSet(instance.getAliasNames()), elementName,
                            Util.sortSet(instance.currentStates), methodName, line.pos.source.toString(), line.pos.line)
                        }
                        newSetOfStates = newSetOfStates ++ newStates
                      }
                      else {
                        instance.currentStates = Set(State(Util.Unknown, -2))
                      }
                    }
                    instance.currentStates = newSetOfStates
                  }
                case None =>
              }
            case None =>
          }
        }
        //reset the traverser's list to be empty
        methodTraverser.methodCallInfos = ListBuffer[Array[String]]()
        println("instances at the end of update if needed are "+instances)
        instances
      }

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


      /** Traverses a tree and collects classes and objects found */
      object classAndObjectTraverser extends Traverser{
        var classesAndObjects = ListBuffer[ClassOrObject]()
        override def traverse(tree: Tree): Unit = {
          println("tree is "+tree)
          tree match{
            case obj@q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
              println("matched object def "+obj)
              classesAndObjects += ClassOrObject(tname.toString, ArrayBuffer(), body, getScope(obj), isObject = true)
              super.traverse(obj)
            case cla@q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
              println("matched class def "+cla)
              val parameters = getParametersWithInstanceNames(paramss)
              classesAndObjects += ClassOrObject(tpname.toString(), parameters, stats, getScope(cla))
              super.traverse(cla)
            case _ =>
              println("matched nothing")
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
                functions += Function(tname.toString(), parameters, tpt, expr, getScope(func), Map[(String, Set[State]), Set[State]]())
              super.traverse(expr)
            case _ =>
              super.traverse(tree)
          }
        }
      }

      /** Removes alias from instances */
      def removeAliases(instances:Set[Instance], aliasName:String): Set[Instance] ={
        println(s"instances before removing $aliasName are "+ instances)
        var newInstances = for(instance <- instances) yield instance
        getClosestScopeAliasInfo(aliasName, newInstances) match{
          case Some(aliasInfo) =>
            val instancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(aliasInfo._1, aliasInfo._2))
            for(instance <- instancesToUpdate)
              instance.aliases -= Alias(aliasInfo._1, aliasInfo._2)
          case None =>
        }
        println(s"instances after removing $aliasName are "+newInstances)
        Util.cleanInstances(newInstances)
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
      def getScope(obj:Tree, dontCheckSymbolField:Boolean = false): mutable.Stack[String] ={
        var objectScope = mutable.Stack[String]()
        if(obj.hasSymbolField || dontCheckSymbolField) {
          for (symbol <- obj.symbol.owner.ownerChain.reverse)
            objectScope.push(symbol.name.toString())
        }
        objectScope
      }

      /** Gets a parameter string as formatted in a function definition from a tree of them */
      def getParametersFromTree(params:List[List[Tree]]): String={
        params match{
          case List(List()) => "()"
          case List(List(value)) => Util.keepOnlyMethodName(value.tpe.toString()).mkString("(","",")")
          case List(values) => {
            var parameters:ArrayBuffer[String] = ArrayBuffer()
            for(elem <- values){
              parameters += Util.keepOnlyMethodName(elem.tpe.toString)
            }
            parameters.mkString("(",",",")")
          }
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
      def checkProtocolMethodsSubsetClassMethods(returnValuesArray:Array[ReturnValue], stats: Seq[Trees#Tree], elementName:String, filename:String): Unit ={
        val classMethodSignatures = getMethodNames(stats)
        println(s"\n$classMethodSignatures")
        var protocolMethodSignatures: Set[String] = Set()
        for(i <- returnValuesArray.indices){
          protocolMethodSignatures += Util.stripReturnValue(returnValuesArray(i).parentMethod.name.replaceAll("\\s", ""))
        }
        println(protocolMethodSignatures)
        if(!(protocolMethodSignatures subsetOf classMethodSignatures)) throw new badlyDefinedProtocolException(
          s"Methods $protocolMethodSignatures defined in $filename are not a subset of methods " +
            s"$classMethodSignatures defined in class $elementName")
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
       * @param methodBody
       * @return
       */
      def getMethodNames(methodBody: Seq[Trees#Tree]): Set[String]={
        var methodNames: Set[String] = Set()
        for(line <- methodBody){
          line match{
            case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" => {
              val parameters = getParameters(paramss) 
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
      

      /** Gets parameters from a tree as their name and type in a string array */
      def getParametersWithInstanceNames(params:List[List[ValDef]]): ArrayBuffer[Array[String]] ={
        params match{
          case List(List()) => ArrayBuffer(Array(""))
          case List(List(value)) => ArrayBuffer(Array(value.name.toString(), value.tpt.toString()))
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
    }
}







