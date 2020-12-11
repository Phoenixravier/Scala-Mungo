package testProtocols

import ProtocolDSL.ProtocolLang

object enumProtocol extends ProtocolLang with App {
  in("init")
  when("m()") goto
    "S2" at "letters.A" or
    "S3" at "letters.B" or
    "S4" at "letters.C" or
    "S5" at "letters.D"
  in("S2")
  when("go()") goto "S6"
  in("S3")
  when("grab()") goto "S8"
  in("S4")
  when("stop()") goto "S7"
  in("S5")
  when("jump()") goto "S9"
  in("S6")
  when("jump()") goto "end"
  in("S7")
  when("jump()") goto "end"
  in("S8")
  when("jump()") goto "end"
  in("S9")
  when("jump()") goto "end"
  in("end")
  end()
}
