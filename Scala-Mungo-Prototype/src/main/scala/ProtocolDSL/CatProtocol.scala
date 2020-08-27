package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    when ("comeAlive()") goto "State1"
    in ("State1")
    when("walk()") goto "State2"
    in ("State2")
    when("walk()") goto "State1"
    end()
}
