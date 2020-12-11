package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in("init")
    when("walk()") goto "end"
    when("jump()") goto "S1"
    in("S1")
    when("jump()") goto "init"
    in("end")
    end()
}
