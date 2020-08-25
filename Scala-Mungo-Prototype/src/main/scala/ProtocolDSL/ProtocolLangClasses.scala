package ProtocolDSL

/**
 * Stores the method, name and index into the array of a return value
 * Implements equals such that two return values are equal if their method name and own name are equal
 * Implements compare on index
 *
 * @param parentMethod
 * @param valueName
 * @param index
 */
@SerialVersionUID(124L)
case class ReturnValue(parentMethod:Method, var valueName:String, var index:Int) extends Ordered[ReturnValue] with Serializable{
  override def equals(returnValue:Any): Boolean ={
    returnValue match{
      case r:ReturnValue => {
        if(r.parentMethod.name.equals(parentMethod.name) && r.canEqual(this)){
          if(r.valueName == null && valueName == null) true
          else if(r.valueName != null && valueName != null) r.valueName.equals(valueName)
          else false
        } else false
      }
      case _ => false
    }
  }

  override def hashCode():Int={
    if(valueName != null) parentMethod.name.hashCode + valueName.hashCode
    parentMethod.name.hashCode
  }

  override def compare(that:ReturnValue) ={
    this.index compare that.index
  }
  override def toString()={
    parentMethod.name + ":" + valueName
  }
}

/** Stores the name and index in the array of a State
 *  Implements compare on index
 * @param name
 * @param index
 */
@SerialVersionUID(123L)
case class State(name: String, index: Int) extends Ordered[State] with Serializable{
  override def compare(that:State) ={
    this.index compare that.index
  }
  override def toString() ={
    name
  }
}

/** Stores name, state it is being used in and indices into the array (might be multiple from multiple return types)
 *  Implements equals on name
 *
 * @param name
 * @param currentState
 * @param indices
 */
@SerialVersionUID(120L)
case class Method(name: String, var currentState: State, var indices:Set[Int] = Set()) extends Serializable{
  override def equals(method:Any): Boolean ={
    method match{
      case m:Method => m.name.equals(name) && m.canEqual(this)
      case _ => false
    }
  }

  override def hashCode():Int={
    name.hashCode
  }
}

/** Stores the starting state, method, return value and next state in a Transition
 *
 * @param startState
 * @param method
 * @param returnValue
 * @param nextState
 */
case class Transition(startState:State, var method:Method, var returnValue:ReturnValue, nextState:String)

