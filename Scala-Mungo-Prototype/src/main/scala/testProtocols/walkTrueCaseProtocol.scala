package testProtocols

import ProtocolDSL.ProtocolLang

object walkTrueCaseProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto
    "end" at "true" or
    "S2" at "Any"
  in("S2")
  when("walk()") goto "end"
  in("end")
  end()
}
