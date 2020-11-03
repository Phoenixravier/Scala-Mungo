package ProtocolDSL

object SomeProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    when("comeAlive()") goto "State2"
    in ("State1")
    in ("State2")
    when("comeAlive()") goto "init"
    end()
}
