package compilerPlugin
import ProtocolDSL.{ReturnValue, State}

import scala.collection.{SortedSet, mutable}

/** Holds an alias' name and scope */
@SerialVersionUID(131L)
case class Alias(var name:String, var scope: mutable.Stack[String]) extends Serializable {
  override def clone(): Alias = super.clone().asInstanceOf[Alias]

  override def toString(): String={
    val printableScope = ""//if(scope != null) scope.reverse.mkString(".") else "null"
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
@SerialVersionUID(130L)
case class Instance(alias:Alias, var currentStates:Set[State], var fields:mutable.Map[Alias, Set[Instance]], var id:Int = 0) extends Serializable{
  def containsFieldName(fieldName: String):Boolean = {
    for(field <- fields.keys){
      if(field.name == fieldName)
        return true
    }
    false
  }

  def getAliasName(): String ={
    alias.name
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Instance => that.canEqual(this) &&
        this.hashCode == that.hashCode
      case _ => false
    }
  }

  def updateState(stateToRemove:State, stateToAdd:State): Unit ={
    currentStates -= stateToRemove
    currentStates += stateToAdd
  }

  def containsAliasInfo(aliasName:String, aliasScope:mutable.Stack[String]): Boolean ={
    alias == Alias(aliasName, aliasScope)
  }

  override def toString(): String={
    if(alias == null)
      return s"null $currentStates"
    var fieldsString = ""
    for((name, instances) <- fields){
      if(instances != null && instances.nonEmpty) {
        val instanceNames =
          for(instance <- instances if instance.alias != null)
            yield s"${instance.alias.name+"@"+instance.id} ${instance.currentStates}"
        fieldsString += s"  $name -> $instanceNames\n"
      } else
        fieldsString += s"  $name -> null\n"
    }
    s"${alias.name+"@"+id} ${alias.scope} $currentStates \n Fields:\n $fieldsString"
  }

  override def hashCode():Int={
    if(alias == null) return id.hashCode()
    (alias.name.hashCode + id.hashCode()).hashCode()
  }
}

/** Holds information about a class or an object with protocol*/
case class ElementInfo(transitions: Array[Array[State]], states: Array[State], methodToIndices: mutable.HashMap[String, Set[Int]],
                       returnValueToIndice: mutable.HashMap[String, Int], stateToAvailableMethods: mutable.HashMap[State, Set[ReturnValue]],
                       var instances: Set[Instance], var objectName: String=null){
  override def toString(): String={
    if(false)
      s"${transitions.foreach(_.mkString(", "))} ${states.mkString(", ")} $methodToIndices $instances $objectName"
    else
      s"$instances"
  }

  override def equals(other: Any): Boolean = {
    other match {
      case (e2: ElementInfo) =>
        false
        // TODO: Implement equality checks for all fields
        // FIXME: sameElements is not enough for transitions, it does a shallow compare on the arrays, so we need a deep compare instead
      case _ => false
    }
  }
}


/** Error for when a protocol is violated */
class protocolViolatedException(aliasNames:SortedSet[String], className:String, states:SortedSet[State], methodName:String,
                                file:String, line:Int, nextMethods: String)
  extends Exception(s"Invalid transition in instance with alias(es) $aliasNames of type $className " +
    s"from state(s) $states with method $methodName in file $file at line $line. Possible methods to use in this state " +
    s"are: $nextMethods")

/** Error for when a field is used uninitialised */
class usedUninitialisedException(method:String, aliases:SortedSet[String], instType:String, line:Int) extends
  Exception(s"Called method $method at line $line on instance with aliases $aliases of type $instType when it was uninitialised.")

/** Error for when the user defines their protocol wrong */
class badlyDefinedProtocolException(message:String) extends Exception(message)

class inconsistentStateMutation(methodName:String, aliasName:String, file:String, line:Int,
                                expectedStates:Set[State], actualStates:Set[State])
  extends Exception(s"In file $file, at line $line, method $methodName changed the state of $aliasName, and not " +
    s"as described in the protocol. Expected states $expectedStates, got states $actualStates")

class unendedProtocolException(instanceType:String, aliases:SortedSet[String], states:SortedSet[State])
  extends Exception(s"Instance of type $instanceType with aliases $aliases did not necessarily " +
    s"reach its end state. At the end of the program it was in state(s) $states")
