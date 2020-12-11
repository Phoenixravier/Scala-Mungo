package testProtocols

import ProtocolDSL.ProtocolLang

object decisionWalkProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto
    "end" at "true" or
    "init" at "false"
  in("end")
  end()
}
