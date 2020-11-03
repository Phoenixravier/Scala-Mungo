import ProtocolDSL.ProtocolLang
object FProtocol extends ProtocolLang with App { 
in("init")
when("receive_checkintFromR()") goto "State1"
in("State1")
when("send_APPROVEToR()") goto "State2"
when("send_REFUSEToR()") goto "State6"
in("State2")
when("send_approveintToR(Int)") goto "State3"
in("State3")
when("send_approveintToA(Int)") goto "State4"
in("State4")
when("receive_invoiceintFromA()") goto "State5"
in("State5")
when("send_paymentintToA(Int)") goto "end"
in("State6")
when("send_refuseStringToR(String)") goto "State7"
in("State7")
when("send_refuseStringToA(String)") goto "end"
in("end")
end()
}