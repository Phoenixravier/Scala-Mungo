package testProtocols

import ProtocolDSL.ProtocolLang

object walkLoop3comeAliveLoop1Protocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto "State1"
  when("comeAlive()") goto "init"
  in("State1")
  when("walk()") goto "end"
  in("end")
  when("walk()") goto "init"
  end()
}
