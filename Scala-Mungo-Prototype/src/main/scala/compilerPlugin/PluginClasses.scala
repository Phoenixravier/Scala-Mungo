package compilerPlugin

import ProtocolDSL.State

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}
import scala.reflect.api.Trees

/** Holds an alias' name and scope */
case class Alias(var name:String, var scope: mutable.Stack[String], instance:Instance){
  override def toString(): String={
    s"$name ${scope.reverse.mkString(".")} ${instance.className}"
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
case class Instance(var className: String, var aliases:Set[Alias], var currentStates:Set[State]){
  def getAliasNames(): Set[String] ={
    for(alias <- aliases) yield alias.name
  }

  def updateState(stateToRemove:State, stateToAdd:State): Unit ={
    currentStates -= stateToRemove
    currentStates += stateToAdd
  }

  def containsAliasInfo(aliasName:String, aliasScope:mutable.Stack[String]): Boolean ={
    aliases.contains(Alias(aliasName, aliasScope, this))
  }


  override def toString(): String={
    s"$className $aliases $currentStates"
  }

  override def equals(instance:Any): Boolean ={
    instance match{
      case i:Instance => i.canEqual(this) && i.aliases.equals(aliases) &&  i.className.equals(className)
      case _ => false
    }
  }

  override def hashCode():Int={
    aliases.hashCode + className.hashCode
  }
}

/** Holds information about a class or an object */
case class ElementInfo(name:String, scope:mutable.Stack[String], transitions:Array[Array[State]], states:Array[State],
                       methodToIndices:mutable.HashMap[String, Set[Int]], isObject:Boolean=false){
  override def toString(): String={
    s"$name $scope ${transitions.foreach(_.mkString(", "))} ${states.mkString(", ")} $methodToIndices $isObject"
  }
}


/** Error for when a protocol is violated */
class protocolViolatedException(aliasNames:SortedSet[String], className:String, states:SortedSet[State], methodName:String,
                                file:String, line:Int)
  extends Exception(s"Invalid transition in instance with alias(es) $aliasNames of type $className " +
    s"from state(s) $states with method $methodName in file $file at line $line")

/** Error for when the user defines their protocol wrong */
class badlyDefinedProtocolException(message:String) extends Exception(message)

case class ClassOrObject(name:String, params:ArrayBuffer[Array[String]], body:Seq[Trees#Tree], scope:mutable.Stack[String],
                         isObject:Boolean=false, var initialised:Boolean=false){
  override def toString(): String={ s"$name ${showParams(params)} $scope $initialised" }

  def showParams(params:ArrayBuffer[Array[String]]):String={
    var parameters = ""
    for(param <- params) {
      for(par <- param)
        parameters += par+": "
      parameters += " ; "
    }
    parameters
  }
}
