import ProtocolDSL.ProtocolLang
object StackUserProtocol extends ProtocolLang with App {
  in("init")
  when("produce(Stack, Int)") goto "Consume"
  when("produce(Stack)") goto "Consume"
  when("close()") goto "end"
  in("Consume")
  when("produce(Stack, Int)") goto "Consume"
  when("produce(Stack)") goto "Consume"
  when("consume(Stack)") goto "init"
  in("end")
  end()
}