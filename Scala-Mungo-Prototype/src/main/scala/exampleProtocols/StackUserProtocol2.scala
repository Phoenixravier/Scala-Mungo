

import ProtocolDSL.ProtocolLang
object StackUserProtocol2 extends ProtocolLang with App {
in("init")
when("produce(Int)") goto "Consume"
when("produce()") goto "Consume"
when("close()") goto "end"
in("Consume")
when("produce(Int)") goto "Consume"
when("produce()") goto "Consume"
when("consume()") goto "init"
in("end")
end()
}