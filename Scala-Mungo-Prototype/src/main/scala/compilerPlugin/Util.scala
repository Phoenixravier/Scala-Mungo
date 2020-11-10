package compilerPlugin

import java.io.{FileInputStream, ObjectInputStream}

import ProtocolDSL.{ReturnValue, State}

import scala.sys.process._
import scala.collection.{SortedSet, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees

object Util {



  val Undefined = "_Undefined_"
  var currentScope:mutable.Stack[String] = mutable.Stack()

  /** Returns protocol data from a file */
  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close
    stock
  }

  /** Removes all instances with an empty set of aliases */
  def cleanInstances(instances:Set[Instance]): Set[Instance]={
    var newInstances = for(instance <- instances) yield instance
    for(instance <- newInstances if instance.aliases.isEmpty) newInstances -= instance
    newInstances
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
    Util.cleanInstances(newInstances)
  }



  def copyInstances(instances:Set[Instance]):Set[Instance]={
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
      newInstances += Instance(instance.className, aliases, states)
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
            mergedInstances += Instance(firstInstance.className, firstInstance.aliases ++ instance.aliases,
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



}


