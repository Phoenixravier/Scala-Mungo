package testProtocols

import ProtocolDSL.ProtocolLang

object withNotAMethodProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto "end"
  in("end")
  when("notAMethod()") goto "end"
  end()
}
