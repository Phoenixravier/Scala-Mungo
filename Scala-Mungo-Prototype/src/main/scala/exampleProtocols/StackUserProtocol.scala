import ProtocolDSL.ProtocolLang

object A{
  class B {
    object StackUserProtocol extends ProtocolLang{
        in("init")
        when("produce(Stack,Int)") goto "Consume"
        when("produce(Stack)") goto "Consume"
        when("close()") goto "end"
        in("Consume")
        when("produce(Stack,Int)") goto "Consume"
        when("produce(Stack)") goto "Consume"
        when("consume(Stack)") goto "init"
        in("end")
        end()
    }
  }
}

object D extends App{
  (new A.B).StackUserProtocol
}