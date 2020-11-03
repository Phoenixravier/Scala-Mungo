package ProtocolDSL

object SomeProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    when("comeAlive()") goto
      "State2" at "true" or
      "State1" at "false"
    in ("State1")
    in ("State2")
    when("comeAlive()") goto
      "init" at "true" or
      "State1" at "false"
    end()
}
