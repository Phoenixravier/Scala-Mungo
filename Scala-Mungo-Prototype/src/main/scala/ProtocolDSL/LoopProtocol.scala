package ProtocolDSL

object LoopProtocol extends ProtocolLang with App {
  in("init")
  when("finished()") goto "init" at "false" or "end" at "true"
  in("end")
  end()
}
