package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in("init")
    when("walk()") goto "end"
    in("end")
    end()
}
