package ProtocolDSL

import scala.language.postfixOps


object Example extends ProtocolLang{
  def main(args:Array[String]) = {
    in ("State0")
    when ("walk(): Unit") goto "State3"
    when ("comeAlive(): Unit") goto "State0"
    when ("die(): Boolean") goto "State3" at "True" or "State2" at "False"

    in ("State3")

    end

  }

}





