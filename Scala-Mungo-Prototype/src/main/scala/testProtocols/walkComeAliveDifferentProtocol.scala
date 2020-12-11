package testProtocols

import ProtocolDSL.ProtocolLang

object walkComeAliveDifferentProtocol extends ProtocolLang with App {
  in("init")
  when("walk()") goto "State1"
  when("comeAlive()") goto "State2"
  in("State1")
  when("walk()") goto "end"
  in("State2")
  when("walk()") goto "end"
  in("end")
  end()
}
