package ProtocolDSL

@SerialVersionUID(124L)
case class ReturnValue(parentMethod:Method, var valueName:String, index:Int) extends Ordered[ReturnValue] with Serializable{
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
    parentMethod.name + ":" + valueName + " " + index
  }
}

@SerialVersionUID(123L)
case class State(name: String, index: Int) extends Ordered[State] with Serializable{
  override def compare(that:State) ={
    this.index compare that.index
  }
  override def toString() ={
    name + " " + index
  }
}

@SerialVersionUID(120L)
case class Method(name: String, var indices:Set[Int] = Set()) extends Serializable{
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

