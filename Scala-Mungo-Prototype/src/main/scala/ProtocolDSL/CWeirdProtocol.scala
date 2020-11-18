package ProtocolDSL

object CWeirdProtocol extends ProtocolLang with App{
  in("init")
  when("m()") goto "S1"
  in("S1")
  end()
}
