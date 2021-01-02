package ProtocolDSL

object CatProtocol extends ProtocolLang with App{
    in("init")
    when("walk()") goto
      "end" at "true" or
      "friendSetState" at "false"
    when("setFriend(Cat)") goto "friendSetState"
    in("friendSetState")
    when("walkWithFriend") goto "end"
    in("end")
    end()
}
