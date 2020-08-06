package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in ("State0")
    when ("walk(): Unit") goto "State3"
    when ("comeAlive(String, Int): String") goto "State0"
    when ("die(): DeathState") goto
      "State1" at "Dead" or
      "State2" at "Alive" or
      "State3" at "Unsure" or
      "State1" at null

    in ("State3")
    in ("State2")
    in ("State1")
    end
}
