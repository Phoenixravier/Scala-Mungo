package ProtocolDSL

object Example extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto
      "State1" at "true" or
      "init" at "false"
    in ("State1")
    end()
}