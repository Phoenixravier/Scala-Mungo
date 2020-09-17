package ProtocolDSL

object Example extends ProtocolLang with App{
    in ("init")
    when ("comeAlive()") goto "init"
    in ("State3")
    when("walk()") goto "init"
    end()
}