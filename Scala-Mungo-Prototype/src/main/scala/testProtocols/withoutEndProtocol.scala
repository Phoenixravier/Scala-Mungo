package testProtocols

import ProtocolDSL.ProtocolLang

object withoutEndProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto "end"
  in("end")
}
