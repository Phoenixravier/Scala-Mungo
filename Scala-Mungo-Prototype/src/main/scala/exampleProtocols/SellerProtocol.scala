import ProtocolDSL.ProtocolLang
object SellerProtocol extends ProtocolLang with App { 
in("init")
when("receive_bookStringFromBuyer1()") goto "State1"
in("State1")
when("send_bookintToBuyer1(int)") goto "State2"
in("State2")
when("receive_Choice1LabelFromBuyer2()") goto
"State3" at "AGREE" or
"State6" at "QUIT" 
in("State3")
when("receive_agreeStringFromBuyer2()") goto "State4"
in("State4")
when("receive_transferintFromBuyer1()") goto "State5"
in("State5")
when("receive_transferintFromBuyer2()") goto "end"
in("State6")
when("receive_quitStringFromBuyer2()") goto "end"
in("end")
end()
}