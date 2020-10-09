package ProtocolDSL

object walkTwiceIllegalProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    in ("State1")
    end()
}