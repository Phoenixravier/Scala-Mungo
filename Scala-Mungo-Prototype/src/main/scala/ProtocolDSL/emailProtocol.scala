package ProtocolDSL

object emailProtocol extends ProtocolLang with App{
  in("init")
  when("start()") goto "Unauthenticated"
  in("Unauthenticated")
  when("authenticate()") goto "Authenticated"
  when("stop()") goto "end"
  in("Authenticated")
  when("logout()") goto "Unauthenticated"
  when("checkEmail()") goto "Authenticated"
  in("end")
  end()
}
