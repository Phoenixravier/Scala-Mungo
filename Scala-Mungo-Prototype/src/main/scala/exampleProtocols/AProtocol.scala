import ProtocolDSL.ProtocolLang
object AProtocol extends ProtocolLang with App { 
in("init")
when("receive_requestStringFromR()") goto "State1"
in("State1")
when("send_quoteintToR(int)") goto "State2"
in("State2")
when("receive_Choice1LabelFromF()") goto
"State3" at "APPROVE" or
"State7" at "REFUSE" 
in("State3")
when("receive_approveintFromF()") goto "State4"
in("State4")
when("send_ticketStringToR(String)") goto "State5"
in("State5")
when("send_invoiceintToF(int)") goto "State6"
in("State6")
when("receive_paymentintFromF()") goto "end"
in("State7")
when("receive_refuseStringFromF()") goto "end"
in("end")
end()
}