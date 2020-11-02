import ProtocolDSL.ProtocolLang
object SMTPSProtocol extends ProtocolLang with App { 
in("init")
when("send_220StringToC(String)") goto "State1"
in("State1")
when("receive_SChoice1LabelFromC()") goto
"State2" at "EHLO" or
"State39" at "QUIT" 
in("State2")
when("receive_ehloStringFromC()") goto "State3"
in("State3")
when("send_250DASHToC()") goto "State4"
when("send_250ToC()") goto "State5"
in("State4")
when("send_250dashStringToC(String)") goto "State3"
in("State5")
when("send_250StringToC(String)") goto "State6"
in("State6")
when("receive_SChoice2LabelFromC()") goto
"State7" at "STARTTLS" or
"State38" at "QUIT" 
in("State7")
when("receive_starttlsStringFromC()") goto "State8"
in("State8")
when("send_220StringToC(String)") goto "State9"
in("State9")
when("receive_SChoice1LabelFromC()") goto
"State10" at "EHLO" or
"State37" at "QUIT" 
in("State10")
when("receive_ehloStringFromC()") goto "State11"
in("State11")
when("send_250DASHToC()") goto "State12"
when("send_250ToC()") goto "State13"
in("State12")
when("send_250dashStringToC(String)") goto "State11"
in("State13")
when("send_250StringToC(String)") goto "State14"
in("State14")
when("receive_SChoice3LabelFromC()") goto
"State15" at "AUTH" or
"State36" at "QUIT" 
in("State15")
when("receive_authStringFromC()") goto "State16"
in("State16")
when("send_235ToC()") goto "State17"
when("send_535ToC()") goto "State35"
in("State17")
when("send_235StringToC(String)") goto "State18"
in("State18")
when("receive_SChoice4LabelFromC()") goto
"State19" at "MAIL" or
"State34" at "QUIT" 
in("State19")
when("receive_mailStringFromC()") goto "State20"
in("State20")
when("send_501ToC()") goto "State21"
when("send_250ToC()") goto "State22"
in("State21")
when("send_501StringToC(String)") goto "State18"
in("State22")
when("send_250StringToC(String)") goto "State23"
in("State23")
when("receive_SChoice5LabelFromC()") goto
"State24" at "RCPT" or
"State27" at "DATA" 
in("State24")
when("receive_rcptStringFromC()") goto "State25"
in("State25")
when("send_250ToC()") goto "State26"
in("State26")
when("send_250StringToC(String)") goto "State23"
in("State27")
when("receive_dataStringFromC()") goto "State28"
in("State28")
when("send_354StringToC(String)") goto "State29"
in("State29")
when("receive_SChoice6LabelFromC()") goto
"State30" at "DATALINE" or
"State31" at "SUBJECT" or
"State32" at "TAD" 
in("State30")
when("receive_datalineStringFromC()") goto "State29"
in("State31")
when("receive_subjectStringFromC()") goto "State29"
in("State32")
when("receive_atadStringFromC()") goto "State33"
in("State33")
when("send_250StringToC(String)") goto "State18"
in("State34")
when("receive_quitStringFromC()") goto "end"
in("State35")
when("send_535StringToC(String)") goto "State14"
in("State36")
when("receive_quitStringFromC()") goto "end"
in("State37")
when("receive_quitStringFromC()") goto "end"
in("State38")
when("receive_quitStringFromC()") goto "end"
in("State39")
when("receive_quitStringFromC()") goto "end"
in("end")
end()
}