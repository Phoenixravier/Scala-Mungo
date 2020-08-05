package ProtocolDSL

case class Method(name: String, var indices:Set[Int] = Set())

case class ReturnValue(parentMethod:Method, var valueName:String, index:Int) extends Ordered[ReturnValue] with Serializable{
  override def compare(that:ReturnValue) ={
    this.index compare that.index
  }
  override def toString()={
    parentMethod.name + ":"+ valueName
  }
}
