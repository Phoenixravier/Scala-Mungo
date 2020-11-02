import ProtocolDSL.ProtocolLang
object AdderCProtocol extends ProtocolLang with App { 
in("init")
when("send_ADDToS()") goto "State1"
when("send_BYEToS()") goto "State4"
in("State1")
when("send_AddintToS(Integer)") goto "State2"
in("State2")
when("send_AddintToS(Integer)") goto "State3"
in("State3")
when("receive_ResintFromS()") goto "init"
in("State4")
when("send_ByeToS()") goto "end"
in("end")
end()
}