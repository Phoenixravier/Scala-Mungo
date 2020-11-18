package exampleProtocols

import ProtocolDSL.ProtocolLang

object MoneyStashProtocol extends ProtocolLang with App{
  in("init")
  when("fill()") goto "filled"
  in("filled")
  when("get()") goto "init"
  end()
}
