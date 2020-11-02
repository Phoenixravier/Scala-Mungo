import ProtocolDSL.ProtocolLang
object RProtocol extends ProtocolLang with App { 
in("init")
when("send_requestStringToA(String)") goto "State1"
in("State1")
when("receive_quoteintFromA()") goto "State2"
in("State2")
when("send_checkintToF(int)") goto "State3"
in("State3")
when("receive_Choice1LabelFromF()") goto
"State4" at "APPROVE" or
"State6" at "REFUSE" 
in("State4")
when("receive_approveintFromF()") goto "State5"
in("State5")
when("receive_ticketStringFromA()") goto "end"
in("State6")
when("receive_refuseStringFromF()") goto "end"
in("end")
end()
}