package testProtocols

import ProtocolDSL.ProtocolLang

object pairsProtocol extends ProtocolLang with App {
  in("init")
  when("setLeft(Int)") goto "leftInitialised"

  in("leftInitialised")
  when("setRight(Int)") goto "allInitialised"

  in("allInitialised")
  when("sum()") goto "allInitialised"

  end()
}
