package compilerPlugin
import ProtocolDSL.{ReturnValue, State}

import scala.collection.{SortedSet, mutable}

/** Holds an alias' name and scope */
case class Alias(var name:String, var scope: mutable.Stack[String]) extends Cloneable {
  override def clone(): Alias = super.clone().asInstanceOf[Alias]

  override def toString(): String={
    val printableScope = if(scope != null) scope.reverse.mkString(".") else "null"
    s"$name $printableScope"
  }

  override def equals(alias:Any): Boolean ={
    alias match{
      case a:Alias => a.canEqual(this) && a.name.equals(name) &&  a.scope.equals(scope)
      case _ => false
    }
  }

  override def hashCode():Int={
    name.hashCode + scope.hashCode
  }
}

/** Holds an instance classname, its aliases and its current possible states */
case class Instance(alias:Alias, var currentStates:Set[State], var fields:mutable.Map[Alias, Set[Instance]], var id:Int = 0){
  def getAliasName(): String ={
    alias.name
  }

  def updateState(stateToRemove:State, stateToAdd:State): Unit ={
    currentStates -= stateToRemove
    currentStates += stateToAdd
  }

  def containsAliasInfo(aliasName:String, aliasScope:mutable.Stack[String]): Boolean ={
    alias == Alias(aliasName, aliasScope)
  }

  def containsScopeAlias(): Boolean ={
    alias != null && alias.name == "scope+"
  }

  override def toString(): String={
    var fieldsString = ""
    for((name, instances) <- fields){
      if(instances != null && instances.nonEmpty)
        fieldsString += s" $name -> $instances\n"
      else
        fieldsString += s" $name -> null\n"
    }
    if(alias != null)
      s"${alias.name+"@"+id} ${alias.scope} $currentStates \n Fields:\n $fieldsString"
    else
      s"null $currentStates"
  }

  override def hashCode():Int={
    alias.hashCode
  }
}

/** Holds information about a class or an object with protocol*/
case class ElementInfo(transitions:Array[Array[State]], states:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]],
                       returnValueToIndice:mutable.HashMap[String, Int], stateToAvailableMethods: mutable.HashMap[State, Set[ReturnValue]],
                       var instances:Set[Instance], var objectName:String=null){
  override def toString(): String={
    if(transitions != null)
      s"${transitions.foreach(_.mkString(", "))} ${states.mkString(", ")} $methodToIndices $instances $objectName"
    else
      s"$instances"
  }
}


/** Error for when a protocol is violated */
class protocolViolatedException(aliasNames:SortedSet[String], className:String, states:SortedSet[State], methodName:String,
                                file:String, line:Int, nextMethods: String)
  extends Exception(s"Invalid transition in instance with alias(es) $aliasNames of type $className " +
    s"from state(s) $states with method $methodName in file $file at line $line. Possible methods to use in this state " +
    s"are: $nextMethods")

class usedUninitialisedException(method:String, aliases:SortedSet[String], instType:String, line:Int) extends
  Exception(s"Called method $method at line $line on instance with aliases $aliases of type $instType when it was uninitialised.")

/** Error for when the user defines their protocol wrong */
class badlyDefinedProtocolException(message:String) extends Exception(message)

class inconsistentStateMutation(methodName:String, aliasName:String, file:String, line:Int,
                                expectedStates:Set[State], actualStates:Set[State])
  extends Exception(s"In file $file, at line $line, method $methodName changed the state of $aliasName, and not " +
    s"as described in the protocol. Expected states $expectedStates, got states $actualStates")

