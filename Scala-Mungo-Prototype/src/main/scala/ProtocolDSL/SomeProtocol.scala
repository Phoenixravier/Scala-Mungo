package ProtocolDSL

object SomeProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto "State1"
    when ("comeAlive()") goto "init"
    in ("State1")
    when("walk()") goto "State2"
    in ("State2")
    in("torororo")
    when("walk()") goto "init"
    for(x <- 3 to 100) {
        in("State"+x)
        when("_Undefined_()") goto "State" + (x - 1) at "_Undefined"
    }
    end()
}
