import ProtocolDSL.ProtocolLang
object SProtocol extends ProtocolLang with App { 
in("init")
when("receive_SChoice1LabelFromC()") goto
"State1" at "ADD" or
"State4" at "BYE" 
in("State1")
when("receive_AddintFromC()") goto "State2"
in("State2")
when("receive_AddintFromC()") goto "State3"
in("State3")
when("send_ResintToC(Integer)") goto "init"
in("State4")
when("receive_ByeFromC()") goto "init"
in("end")
end()
}