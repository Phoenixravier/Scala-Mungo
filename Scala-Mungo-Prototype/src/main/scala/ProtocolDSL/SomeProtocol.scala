package ProtocolDSL


object SomeProtocol extends ProtocolLang with App{
    in ("init")
    when ("walk()") goto
      "State2" at ("True") or
      "State3" at "True"

    in ("State3")
    when ("walk()") goto
      "State3"

    in ("State2")

    in ("State1")
    when("walk()") goto
      "State2"
    end()
}
