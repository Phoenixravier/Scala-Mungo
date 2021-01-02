package compilerPlugin

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Paths}
import ProtocolDSL.{ReturnValue, State}

import scala.collection.{SortedSet, mutable}
import scala.reflect.api.Trees

object Util {

  val Undefined = "_Undefined_"
  val Any = "_Any_"
  var currentScope:mutable.Stack[String] = mutable.Stack()
  var currentInstance:mutable.Stack[Instance] = mutable.Stack()
  var trackedElements: mutable.Map[String, ElementInfo] = mutable.Map[String, ElementInfo]()
  var userDirectory: String = ""

  /** Removes all instances with an empty set of aliases */
  def cleanInstances(instances:Set[Instance]): Set[Instance]={
    var newInstances = for(instance <- instances) yield instance
    for(instance <- newInstances if instance.alias == null) newInstances -= instance
    newInstances
  }


  /** Initialise all the objects in protocolled objects to a single instance in state init
   *
   */
  def initObjects() = {
    for(elementInfo <- trackedElements.values){
      if(elementInfo.objectName != null) {
        if(elementInfo.states != null)
          elementInfo.instances += Instance(Alias(elementInfo.objectName, currentScope.clone()),
            Set(elementInfo.states(0)), mutable.Map())
        else
          elementInfo.instances += Instance(Alias(elementInfo.objectName, currentScope.clone()),
            Set(), mutable.Map())
      }
    }
  }

  def removeAllInstances(): Any = {
    for(elementInfo <- trackedElements.values){
      elementInfo.instances = Set()
    }
  }

  /** Sorts a set */
  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] = SortedSet.empty[A] ++ unsortedSet

  /** Gets rid of the return value in a method name string and keeps the parenthesis at the end */
  def stripReturnValue(methodName:String): String ={
    //case walk -> walk()
    if(!(methodName.contains(':') || methodName.contains("()") || (methodName.contains("(") && methodName.contains(")"))))
      methodName+"()"
    //cases walk:Int and walk: -> walk()
    else if(methodName.contains(':') && !methodName.contains("(") && !methodName.contains(")")) methodName.substring(0,methodName.indexOf(':'))+"()"
    //cases walk() and walk(Int) -> walk() and walk(Int)
    else if(methodName(methodName.length-1) == ')') methodName
    //cases walk():Int and walk(Int):Int -> walk() and walk(Int)
    else methodName.substring(0,methodName.indexOf(')')+1)
  }

  /** Takes a string a strips everything after, and including ( from it */
  def keepOnlyName(method:String): String ={
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

  /** Gets the length of a tree in nb of lines */
  def getLengthOfTree(tree:Trees#Tree): Int ={
    var length = 0
    for(line <- tree) length +=1
    length
  }


  /** Creates a hashmap from method names (e.g. "walk(String)")
   * to indices at which it is present in the transitions array (including all the return values it might have)
   *
   * @param returnValuesArray:Array[ReturnValue]
   * @return mutable.Hashmap[String, Set[Int]]
   */
  def createMethodToIndicesMap(returnValuesArray:Array[ReturnValue]): mutable.HashMap[String, Set[Int]] ={
    var methodToIndices:mutable.HashMap[String, Set[Int]] = mutable.HashMap()
    for(returnValue <- returnValuesArray){
      methodToIndices += (Util.stripReturnValue(returnValue.parentMethod.name) -> returnValue.parentMethod.indices)
    }
    methodToIndices
  }

  def createReturnValueStringToIndiceMap(returnValuesArray: Array[ReturnValue]): mutable.HashMap[String, Int] = {
    var returnValueToIndice:mutable.HashMap[String, Int] = mutable.HashMap()
    for(returnValue <- returnValuesArray)
      returnValueToIndice += Util.stripReturnValue(returnValue.parentMethod.name) + ":" +returnValue.valueName -> returnValue.index
    returnValueToIndice
  }

  def createReturnValueToIndiceMap(returnValuesArray: Array[ReturnValue]): mutable.HashMap[ReturnValue, Int] = {
    var returnValueToIndice:mutable.HashMap[ReturnValue, Int] = mutable.HashMap()
    for(returnValue <- returnValuesArray)
      returnValueToIndice += returnValue -> returnValue.index
    returnValueToIndice
  }

  def createStateToAvailableMethodsMap(stateMachine: Array[Array[State]], returnValueToIndice:mutable.HashMap[ReturnValue, Int],
                                       states:Array[State]): mutable.HashMap[State, Set[ReturnValue]] = {
    var stateToAvailableMethods:mutable.HashMap[State, Set[ReturnValue]] = mutable.HashMap()
    val indiceToReturnValue = returnValueToIndice.map(_.swap)
    for(state <- states){
      var i = 0
      var methodIndicesForState:Set[Int] = Set()
      println("next states row is "+stateMachine(state.index).mkString("Array(", ", ", ")"))
      for(nextState <- stateMachine(state.index)) {
        if (nextState.name != Undefined)
          methodIndicesForState += i
        i += 1
      }
      var possibleMethods:Set[ReturnValue] = Set()
      for(indice <- methodIndicesForState)
        possibleMethods += indiceToReturnValue(indice)
      stateToAvailableMethods += state -> possibleMethods
    }
    println("state to av is "+stateToAvailableMethods)
    stateToAvailableMethods
  }

  /** From a scope implemented as a stack, gets a string formatted with dots */
  def getScopeString(scopeStack:mutable.Stack[String]): String ={
    scopeStack.reverse.mkString(".")
  }

  def copyInstances(instances:Set[Instance]):Set[Instance]={
    if(instances == null) return null
    var newInstances:Set[Instance] = Set()
    for(instance <- instances){
      var states:Set[State] = Set()
      var fields:mutable.Map[Alias, Set[Instance]] = mutable.Map()
      if(instance.currentStates == null) states = null
      else {
        for (state <- instance.currentStates) {
          if (state == null) states += null
          else states += State(state.name.trim(), state.index)
        }
      }
      for((fieldName, fieldInstances) <- instance.fields )
        fields += fieldName -> copyInstances(fieldInstances)
      newInstances += Instance(instance.alias, states, fields, instance.id)
    }
    newInstances
  }


  def mergeFields(firstInstance: Instance, secondInstance: Instance): mutable.Map[Alias, Set[Instance]] = {
    var newFields = mutable.Map[Alias, Set[Instance]]()
    for((fieldName, instances) <- firstInstance.fields){
      if(secondInstance.fields.contains(fieldName))
        newFields += (fieldName -> (instances ++ secondInstance.fields(fieldName)))
      else
        newFields += (fieldName -> instances)
    }
    for((fieldName, instances) <- secondInstance.fields if !newFields.contains(fieldName)){
      newFields += (fieldName -> instances)
    }
    newFields
  }

  /** For two sets of instances, if and instance is present in both of them, merges the different states
   * associated with it into one instance. Copies over the remaining instances which are only present once.
   *
   * @param firstInstances  First of the instance sets, to merge with the second one
   * @param secondInstances Second of the instance sets, to merge with the first one
   * @return
   */
  def mergeInstanceStates(firstInstances: Set[Instance], secondInstances: Set[Instance]): Set[Instance] = {
    println(s"merging $firstInstances with $secondInstances")
    var mergedInstances: Set[Instance] = Set()
    for (firstInstance <- firstInstances ) {
      val alias = firstInstance.alias
      secondInstances.find(instance => instance.alias == alias) match {
        case Some(instance) =>
          if(firstInstance.currentStates != null)
            mergedInstances += Instance(alias,
              firstInstance.currentStates ++ instance.currentStates, mutable.Map[Alias, Set[Instance]]())
          else mergedInstances += Instance(alias, null, mutable.Map[Alias, Set[Instance]]())
        case None => mergedInstances += firstInstance
      }
    }
    for (secondInstance <- secondInstances) if(!firstInstances.exists(instance => instance.alias == secondInstance.alias)) {
      mergedInstances += Instance(secondInstance.alias, secondInstance.currentStates, mutable.Map[Alias, Set[Instance]]())
    }
    println("merged instances are "+mergedInstances)
    mergedInstances
  }

  /** Merges two instance maps together into one map with instances present in both maps having merges states
   *
   * @param firstMap  first map to merge, order is not important
   * @param secondMap second map to merge, order is not important
   */
  def mergeMaps(firstMap: mutable.Map[String, ElementInfo], secondMap: mutable.Map[String, ElementInfo]): mutable.Map[String, ElementInfo] = {
    println(s"at the top of merge maps, first map is $firstMap and second map is $secondMap")
    var newMap = mutable.Map[String, ElementInfo]()
    for ((elementType, elementInfo) <- firstMap) {
      newMap += (elementType -> copy(elementInfo))
      newMap(elementType).instances = mergeInstanceStates(copyInstances(elementInfo.instances), copyInstances(secondMap(elementType).instances))
    }
    //todo add in the instances from second map
    println("after initial merge, new map is "+newMap)
    println(s"first map is $firstMap, second map is $secondMap")
    newMap = addFields(firstMap, secondMap, newMap)
    println("after adding fields, new map is "+newMap)
    newMap
  }

  def copy(elementInfo: ElementInfo): ElementInfo ={
    ElementInfo(elementInfo.transitions, elementInfo.states, elementInfo.methodToIndices, elementInfo.returnValueToIndice,
      elementInfo.stateToAvailableMethods, copyInstances(elementInfo.instances))
  }

  def addFields(firstMap: mutable.Map[String, ElementInfo], secondMap: mutable.Map[String, ElementInfo],
                newMap: mutable.Map[String, ElementInfo]): mutable.Map[String, ElementInfo] = {
    for((elementType, elementInfo) <- firstMap){
      for(instance <- elementInfo.instances){
        var newMapInstance = newMap(elementType).instances.filter(newInstance => newInstance == instance).last
        for((field, instancesPointedTo) <- instance.fields){
          if(secondMap(elementType).instances.contains(instance)) {
            val secondMapInstance = secondMap(elementType).instances.filter(secondInstance => secondInstance == instance).last
            //if the field exists in both maps, want to link all instances pointed to by both the maps
            if (secondMapInstance.fields.contains(field)) {
              println(s"second map contains field $field")
              println(s"map are $firstMap and $secondMap")
              val secondInstancesPointedTo = secondMapInstance.fields(field)
              println("secondInstancePointedTo are "+secondInstancesPointedTo)
              //get instances pointed to (in first map) from the newMap and same for second map pointed instances
              var instancesToPointTo = Set[Instance]()
              for (instance <- newMap(elementType).instances) {
                if (instancesPointedTo.contains(instance) || secondInstancesPointedTo.contains(instance))
                  instancesToPointTo += instance
              }
              println("instances to point to are "+instancesToPointTo)
              newMapInstance.fields += (field -> instancesToPointTo)
            }
            //if not, want to add the instances pointed to from the first map and null from the second
            else {
              var instancesToPointTo = Set[Instance]()
              for (instance <- newMap(elementType).instances)
                if (instancesPointedTo.contains(instance))
                  instancesToPointTo += instance
              instancesToPointTo += Instance(null, Set(State(Undefined, -1)), null)
              newMapInstance.fields += (field -> instancesToPointTo)
            }
          }
        }
      }
    }
    //todo add case for fields which are in second map but not first
    newMap
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
  def getClosestScopeAliasInfo(name: String, elementType:String): Option[(String, mutable.Stack[String])] = {
    if (elementType != null) {
      if (trackedElements.contains(elementType)) {
        println("found element in tracked elements")
        if (trackedElements(elementType).instances.isEmpty) return None
        println("passed break")
        val curScope = currentScope.clone()
        while (curScope.nonEmpty) {
          for (instance <- trackedElements(elementType).instances) {
             if (instance.alias.name == name && instance.alias.scope == curScope) {
              println(s"returning ${instance.alias}")
              return Some(instance.alias.name, instance.alias.scope)
            }
          }
          curScope.pop()
        }
      }
      None
    }
    None
  }

  /** Prints instances nicely
   *
   */
  def printInstances() = {
    println("\nInstances:")
    for((elementType, elementInfo) <- trackedElements){
      var classOrObject = if(elementInfo.objectName != null) "object" else "class"
      println(s"For $classOrObject $elementType: ")
      for(instance <- elementInfo.instances){
        println(instance)
      }
      println()
    }
    if(currentInstance.nonEmpty) {
      println("current instance is:")
      println(currentInstance.head)
    }
  }


  /** Writes the state machine, the array of states and the array of return values (packaged in data) to a file
   * with name filename.
   * The state and return value arrays are needed to be able to index properly into the state machine.*/
  def sendDataToFile(data: (Array[Array[State]], Array[State], Array[ReturnValue]), filename:String): Unit ={
    println("in send data, user dir is "+sys.props.get("user.dir"))
    val path = Paths.get("compiledProtocols")
    Files.createDirectories(path)
    val oos = new ObjectOutputStream(new FileOutputStream(path+"/"+filename))
    oos.writeObject(data)
    oos.close()
  }

  def getDataFromProtocol(protocolName:String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    println("protocol name is "+protocolName)
    val path = Paths.get("compiledProtocols")
    println("path is "+path)
    println("absolute path is "+path.toFile().getAbsolutePath())
    println(getListOfFiles(path.toString))
    println("userDirectory is "+userDirectory)
    val protocolPath = if(userDirectory == "") s"compiledProtocols/$protocolName.ser" else s"$userDirectory/compiledProtocols/$protocolName.ser"
    println("protocol path is "+protocolPath)
    if (!Files.exists(Paths.get(protocolPath)))
      throw new badlyDefinedProtocolException(s"The protocol $protocolName could not be processed, " +
        s"check that the protocol name is the same as the name of the object containing your protocol")
    getDataFromFile(protocolPath)
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }


  /** Returns protocol data from a file */
  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close()
    stock
  }

}