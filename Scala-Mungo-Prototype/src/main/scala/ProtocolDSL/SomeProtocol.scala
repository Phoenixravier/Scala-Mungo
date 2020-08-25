package ProtocolDSL


object SomeProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto
      "State2" at "undefined"

    in ("State3")
    when ("walk()") goto
      "State3" at "False"

    in ("State2")

    in ("State1")
    when("walk()") goto
      "State2" at "False"
    end()
}
