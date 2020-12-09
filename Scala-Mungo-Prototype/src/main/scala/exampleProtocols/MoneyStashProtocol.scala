package exampleProtocols

import ProtocolDSL.ProtocolLang

object MoneyStashProtocol extends ProtocolLang with App{
  in("init")
  when("fill(Float)") goto "intermediate"
  in("intermediate")
  when("applyInterest(Float)") goto "filled"
  in("filled")
  when("get()") goto "end"
  in("end")
  end()
}
