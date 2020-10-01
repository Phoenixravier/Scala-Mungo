package ProtocolDSL

object withNotAMethodProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    in ("State1")
    when ("notAMethod()") goto "State1"
    end
}