package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto
      "State1" at "true" or
      "init" at "false"
    when("comeAlive()") goto "init"
    in ("State1")
    when ("comeAlive()") goto "State2"
    in ("State2")
    when("walk()") goto "init"
    end()
}
