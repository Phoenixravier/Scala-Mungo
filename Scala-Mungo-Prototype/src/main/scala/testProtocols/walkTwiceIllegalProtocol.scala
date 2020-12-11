package testProtocols

import ProtocolDSL.ProtocolLang

object walkTwiceIllegalProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto "end"
  in("end")
  end()
}
