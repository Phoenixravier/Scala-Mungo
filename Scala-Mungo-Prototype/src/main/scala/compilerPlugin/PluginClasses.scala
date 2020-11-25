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
case class Instance(var aliases:Set[Alias], var currentStates:Set[State], var fields:mutable.Map[String, Set[Instance]]){
  def getAliasNames(): Set[String] ={
    for(alias <- aliases) yield alias.name
  }

  def updateAlias(aliasToRemove:Alias, aliasToAdd:Alias): Unit ={
    aliases -= aliasToRemove
    aliases += aliasToAdd
  }

  def updateState(stateToRemove:State, stateToAdd:State): Unit ={
    currentStates -= stateToRemove
    currentStates += stateToAdd
  }

  def containsAliasInfo(aliasName:String, aliasScope:mutable.Stack[String]): Boolean ={
    aliases.contains(Alias(aliasName, aliasScope))
  }

  def containsScopeAlias(): Boolean ={
    aliases.nonEmpty && aliases.last.name == "scope+"
  }

  override def toString(): String={
    s"$aliases $currentStates $fields"
  }

  override def hashCode():Int={
    aliases.hashCode
  }
}

/** Holds information about a class or an object with protocol*/
case class ElementInfo(transitions:Array[Array[State]], states:Array[State], methodToIndices:mutable.HashMap[String, Set[Int]],
                       returnValueToIndice:mutable.HashMap[String, Int], stateToAvailableMethods: mutable.HashMap[State, Set[ReturnValue]],
                       var instances:Set[Instance], var objectName:String=null){
  override def toString(): String={
    s"${transitions.foreach(_.mkString(", "))} ${states.mkString(", ")} $methodToIndices $instances $objectName"
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

