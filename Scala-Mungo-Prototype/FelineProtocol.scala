package ProtocolDSL

object Example extends ProtocolLang{
  def main(args:Array[String]) = {
    in ("init")
    when ("walk()") goto "State1"
    in ("State1")
    end()
  }
}