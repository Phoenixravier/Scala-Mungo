import ProtocolDSL.ProtocolLang
object SMTPCProtocol extends ProtocolLang with App { 
in("init")
when("receive_220StringFromS()") goto "State1"
in("State1")
when("send_EHLOToS()") goto "State2"
when("send_QUITToS()") goto "State39"
in("State2")
when("send_ehloStringToS(String)") goto "State3"
in("State3")
when("receive_CChoice1LabelFromS()") goto
"State4" at "_250DASH" or
"State5" at "_250" 
in("State4")
when("receive_250dashStringFromS()") goto "State3"
in("State5")
when("receive_250StringFromS()") goto "State6"
in("State6")
when("send_STARTTLSToS()") goto "State7"
when("send_QUITToS()") goto "State38"
in("State7")
when("send_starttlsStringToS(String)") goto "State8"
in("State8")
when("receive_220StringFromS()") goto "State9"
in("State9")
when("send_EHLOToS()") goto "State10"
when("send_QUITToS()") goto "State37"
in("State10")
when("send_ehloStringToS(String)") goto "State11"
in("State11")
when("receive_CChoice1LabelFromS()") goto
"State12" at "_250DASH" or
"State13" at "_250" 
in("State12")
when("receive_250dashStringFromS()") goto "State11"
in("State13")
when("receive_250StringFromS()") goto "State14"
in("State14")
when("send_AUTHToS()") goto "State15"
when("send_QUITToS()") goto "State36"
in("State15")
when("send_authStringToS(String)") goto "State16"
in("State16")
when("receive_CChoice2LabelFromS()") goto
"State17" at "_235" or
"State35" at "_535" 
in("State17")
when("receive_235StringFromS()") goto "State18"
in("State18")
when("send_MAILToS()") goto "State19"
when("send_QUITToS()") goto "State34"
in("State19")
when("send_mailStringToS(String)") goto "State20"
in("State20")
when("receive_CChoice3LabelFromS()") goto
"State21" at "_501" or
"State22" at "_250" 
in("State21")
when("receive_501StringFromS()") goto "State18"
in("State22")
when("receive_250StringFromS()") goto "State23"
in("State23")
when("send_RCPTToS()") goto "State24"
when("send_DATAToS()") goto "State27"
in("State24")
when("send_rcptStringToS(String)") goto "State25"
in("State25")
when("receive_CChoice4LabelFromS()") goto
"State26" at "_250" 
in("State26")
when("receive_250StringFromS()") goto "State23"
in("State27")
when("send_dataStringToS(String)") goto "State28"
in("State28")
when("receive_354StringFromS()") goto "State29"
in("State29")
when("send_DATALINEToS()") goto "State30"
when("send_SUBJECTToS()") goto "State31"
when("send_ATADToS()") goto "State32"
in("State30")
when("send_datalineStringToS(String)") goto "State29"
in("State31")
when("send_subjectStringToS(String)") goto "State29"
in("State32")
when("send_atadStringToS(String)") goto "State33"
in("State33")
when("receive_250StringFromS()") goto "State18"
in("State34")
when("send_quitStringToS(String)") goto "end"
in("State35")
when("receive_535StringFromS()") goto "State14"
in("State36")
when("send_quitStringToS(String)") goto "end"
in("State37")
when("send_quitStringToS(String)") goto "end"
in("State38")
when("send_quitStringToS(String)") goto "end"
in("State39")
when("send_quitStringToS(String)") goto "end"
in("end")
end()
}