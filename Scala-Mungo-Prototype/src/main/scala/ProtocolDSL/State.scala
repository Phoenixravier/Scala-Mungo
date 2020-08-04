package ProtocolDSL

@SerialVersionUID(123L)
case class State(name: String, index: Int) extends Serializable{
  override def toString() ={
    name
  }
}
