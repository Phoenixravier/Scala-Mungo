package testProtocols

import ProtocolDSL.ProtocolLang

object walkTrueCaseProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto
    "S1" at "true" or
    "S2" at "Any"
  in("S1")
  in("S2")
  end()
}
