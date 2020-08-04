package ProtocolDSL

object Example extends ProtocolLang{
  def main(args:Array[String]) = {
    in ("State0")
    when ("walk()") goto "State3"
    when ("comeAlive()") goto "State0"
    when ("die()") goto
      "State1" at "True" or
      "State2" at "False" or
      "State3" at "Maybe" or
      "State1" at null

    in ("State3")
    in ("State2")
    in ("State1")
    end()
    println("done")
  }
}
