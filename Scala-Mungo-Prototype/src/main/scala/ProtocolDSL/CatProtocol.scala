package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in("init")
    when("walk()") goto "end"
    when("jujmp()") goto "State1"
    in("State1")
    when("walk()") goto "end"
    in("end")
    end()
}
