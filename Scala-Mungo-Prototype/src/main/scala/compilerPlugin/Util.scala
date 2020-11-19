package compilerPlugin

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Paths}

import ProtocolDSL.{ReturnValue, State}

import scala.collection.{SortedSet, mutable}
import scala.reflect.api.Trees

object Util {


  val Undefined = "_Undefined_"
  var currentScope:mutable.Stack[String] = mutable.Stack()
  var protocolledElements: mutable.Map[String, ElementInfo] = mutable.Map[String, ElementInfo]()


  /** Removes all instances with an empty set of aliases */
  def cleanInstances(instances:Set[Instance]): Set[Instance]={
    var newInstances = for(instance <- instances) yield instance
    for(instance <- newInstances if instance.aliases.isEmpty) newInstances -= instance
    newInstances
  }


  /** Initialise all the objects in protocolled objects to a single instance in state init
   *
   */
  def initObjects() = {
    for(elementInfo <- protocolledElements.values){
      if(elementInfo.objectName != null)
        elementInfo.instances += Instance(Set(Alias(elementInfo.objectName, currentScope.clone())), Set(elementInfo.states(0)))
    }
  }

  def removeAllInstances(): Any = {
    for(elementInfo <- protocolledElements.values){
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

  /** Searches through instances for an instance with an empty set and adds the alias there */
  def addInMissingAlias(instances: Set[Instance], name: String):Set[Instance] = {
    val newInstances = for(instance <- instances) yield instance
    for(instance <- newInstances if instance.aliases.isEmpty) instance.aliases += Alias(name, currentScope.clone)
    newInstances
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

  def createReturnValueToIndiceMap(returnValuesArray: Array[ReturnValue]): mutable.HashMap[String, Int] = {
    var returnValueToIndice:mutable.HashMap[String, Int] = mutable.HashMap()
    for(returnValue <- returnValuesArray)
      returnValueToIndice += Util.stripReturnValue(returnValue.parentMethod.name) + ":" +returnValue.valueName -> returnValue.index
    returnValueToIndice
  }

  def createStateToAvailableMethodsMap(returnValuesArray: Array[ReturnValue]): mutable.HashMap[State, Set[ReturnValue]] = {
    var stateToAvailableMethods:mutable.HashMap[State, Set[ReturnValue]] = mutable.HashMap()
    for(returnValue <- returnValuesArray){
      if(!stateToAvailableMethods.contains(returnValue.parentMethod.currentState))
        stateToAvailableMethods += returnValue.parentMethod.currentState -> Set(returnValue)
      else
        stateToAvailableMethods(returnValue.parentMethod.currentState) += returnValue
    }
    stateToAvailableMethods
  }

  /** From a scope implemented as a stack, gets a string formatted with dots */
  def getScopeString(scopeStack:mutable.Stack[String]): String ={
    scopeStack.reverse.mkString(".")
  }

  def removeAliasesInScope(instances: Set[Instance], aliasName: String, scope:mutable.Stack[String]): Set[Instance] = {
    var newInstances = for (instance <- instances) yield instance
    val instancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(aliasName, scope))
    for (instance <- instancesToUpdate)
      instance.aliases -= Alias(aliasName, scope)
    cleanInstances(newInstances)
  }



  def copyInstances(instances:Set[Instance]):Set[Instance]={
    if(instances == null) return null
    var newInstances:Set[Instance] = Set()
    for(instance <- instances){
      var aliases:Set[Alias] = Set()
      var states:Set[State] = Set()
      for(alias <- instance.aliases){
        aliases += Alias(alias.name.trim(), alias.scope.clone())
      }
      for(state <- instance.currentStates){
        states += State(state.name.trim(), state.index)
      }
      newInstances += Instance(aliases, states)
    }
    newInstances
  }


  /** For two sets of instances, if and instance is present in both of them, merges the different states
   * associated with it into one instance. Copies over the remaining instances which are only present once.
   *
   * @param firstInstances  First of the instance sets, to merge with hte second one
   * @param secondInstances Second of the instance sets, to merge with the first one
   * @return
   */
  def mergeInstanceStates(firstInstances: Set[Instance], secondInstances: Set[Instance]): Set[Instance] = {
    println(s"merging $firstInstances with $secondInstances")
    var mergedInstances: Set[Instance] = Set()
    for (firstInstance <- firstInstances) {
      for (alias <- firstInstance.aliases) {
        secondInstances.find(instance => instance.aliases.contains(alias)) match {
          case Some(instance) =>
            mergedInstances += Instance(firstInstance.aliases ++ instance.aliases,
              firstInstance.currentStates ++ instance.currentStates)
          case None => mergedInstances += firstInstance
        }
      }
    }
    for (secondInstance <- secondInstances) if(!firstInstances.exists(instance => instance.aliases == secondInstance.aliases)) {
      mergedInstances += secondInstance
    }
    println("merged instances are "+mergedInstances)
    mergedInstances
  }


  /** Removes alias from instances */
  def removeAliases(elementType:String, aliasName: String) = {
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
      if (protocolledElements.contains(elementType)) {
        if (protocolledElements(elementType).instances.isEmpty) return None
        val curScope = currentScope.clone()
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
    None
  }


  /** Writes the state machine, the array of states and the array of return values (packaged in data) to a file
   * with name filename.
   * The state and return value arrays are needed to be able to index properly into the state machine.*/
  def sendDataToFile(data: (Array[Array[State]], Array[State], Array[ReturnValue]), filename:String): Unit ={
    val path = Paths.get("/compiledProtocols/")
    if(!(Files.exists(path) && Files.isDirectory(path)))
      Files.createDirectory(path)
    println(path+"/"+filename)
    println("user dir in util is "+System.getProperty("user.dir"))
    val oos = new ObjectOutputStream(new FileOutputStream(path+"/"+filename))
    oos.writeObject(data)
    oos.close()
    println("file exists is "+Files.exists(Paths.get(s"/compiledProtocols/$filename.ser")))
  }

  def getDataFromProtocol(protocolName:String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    println("user dir in util get data is "+System.getProperty("user.dir"))
    if (!Files.exists(Paths.get(s"/compiledProtocols/$protocolName.ser")))
      throw new badlyDefinedProtocolException(s"The protocol $protocolName could not be processed, " +
        s"check that the protocol name is the same as the name of the object containing your protocol")
    getDataFromFile(s"/compiledProtocols/$protocolName.ser")
  }


  /** Returns protocol data from a file */
  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close()
    stock
  }

}


