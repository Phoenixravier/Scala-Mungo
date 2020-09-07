package ProtocolDSL

object Example extends ProtocolLang{
    def main(args:Array[String]) = {
        in ("init")
        when ("walk()") goto "State3"
        when ("comeAlive()") goto "init"
        when ("die()") goto
          "State1" at "True" or
          "State2" at "False" or
          "State3" at "Maybe" or
          "State1" at null

        in ("State3")
        in ("State2")
        in ("State1")
        end
    }
}
