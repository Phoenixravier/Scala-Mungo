package testProtocols

import ProtocolDSL.ProtocolLang

object cannotWalkProtocol extends ProtocolLang with App {
  in("init")
  when("comeAlive()") goto "State1"
  in("State1")
  when("comeAlive()") goto "State2"
  in("State2")
  when("walk()") goto "end"
  when("comeAlive()") goto "init"
  in("end")
  end()
}
