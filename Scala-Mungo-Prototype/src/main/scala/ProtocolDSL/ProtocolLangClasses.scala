package ProtocolDSL

@SerialVersionUID(124L)
case class ReturnValue(parentMethod:Method, var valueName:String, index:Int) extends Ordered[ReturnValue] with Serializable{
  override def compare(that:ReturnValue) ={
    this.index compare that.index
  }
  override def toString()={
    parentMethod.name + ":" + valueName
  }
}

@SerialVersionUID(123L)
case class State(name: String, index: Int) extends Ordered[State] with Serializable{
  override def compare(that:State) ={
    this.index compare that.index
  }
  override def toString() ={
    name
  }
}

@SerialVersionUID(120L)
case class Method(name: String, var indices:Set[Int] = Set()) extends Serializable

