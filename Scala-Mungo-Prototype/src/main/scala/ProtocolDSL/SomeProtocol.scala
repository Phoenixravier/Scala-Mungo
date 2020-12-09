package ProtocolDSL

object SomeProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    in ("State1")
    when ("walk()") goto "init"
    in("end")
    end()
}
