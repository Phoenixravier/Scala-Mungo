package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in ("init")
    when ("m()") goto
        "S2" at "letters.A" or
        "S3" at "letters.B" or
        "S4" at "letters.C" or
        "S5" at "letters.D"
    in("S2")
    when("go()") goto "S6"
    in("S3")
    when("grab()") goto "S8"
    in("S4")
    when("stop()") goto "S7"
    in("S5")
    when("jump()") goto "S9"
    in("S6")
    in("S7")
    in("S8")
    in("S9")
    end()
}
