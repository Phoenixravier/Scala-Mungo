import ProtocolDSL.ProtocolLang

object Buyer1Protocol extends ProtocolLang with App { 
in("init")
when("send_bookStringToSeller(String)") goto "State1"
in("State1")
when("receive_bookintFromSeller()") goto "State2"
in("State2")
when("send_quoteintToBuyer2(Int)") goto "State3"
in("State3")
when("receive_Choice1LabelFromBuyer2()") goto
"State4" at "AGREE" or
"State6" at "QUIT" 
in("State4")
when("receive_agreeStringFromBuyer2()") goto "State5"
in("State5")
when("send_transferintToSeller(Int)") goto "end"
in("State6")
when("receive_quitStringFromBuyer2()") goto "end"
in("end")
end()
}