import ProtocolDSL.ProtocolLang
object Buyer2Protocol extends ProtocolLang with App { 
in("init")
when("receive_quoteintFromBuyer1()") goto "State1"
in("State1")
when("send_AGREEToBuyer1()") goto "State2"
when("send_QUITToBuyer1()") goto "State5"
in("State2")
when("send_agreeStringToBuyer1(String)") goto "State3"
in("State3")
when("send_agreeStringToSeller(String)") goto "State4"
in("State4")
when("send_transferintToSeller(Int)") goto "end"
in("State5")
when("send_quitStringToBuyer1(String)") goto "State6"
in("State6")
when("send_quitStringToSeller(String)") goto "end"
in("end")
end()
}