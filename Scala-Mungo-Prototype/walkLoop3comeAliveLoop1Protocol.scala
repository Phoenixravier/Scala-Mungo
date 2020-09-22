package ProtocolDSL

object Example extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    when ("comeAlive()") goto "init"
    in ("State1")
    when("walk()") goto "State2"
    in ("State2")
    when("walk()") goto "init"
    end()
}