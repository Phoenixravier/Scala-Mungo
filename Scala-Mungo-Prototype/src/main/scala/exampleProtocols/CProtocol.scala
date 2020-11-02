import ProtocolDSL.ProtocolLang
object CProtocol extends ProtocolLang with App { 
in("init")
when("send_REQUESTToS()") goto "State1"
in("State1")
when("send_requestStrToS(String)") goto "State2"
in("State2")
when("send_HOSTToS()") goto "State3"
when("send_USERAToS()") goto "State4"
when("send_ACCEPTTToS()") goto "State5"
when("send_ACCEPTLToS()") goto "State6"
when("send_ACCEPTEToS()") goto "State7"
when("send_DNTToS()") goto "State8"
when("send_CONNECTIONToS()") goto "State9"
when("send_UPGRADEIRToS()") goto "State10"
when("send_COOKIEToS()") goto "State11"
when("send_BODYToS()") goto "State12"
in("State3")
when("send_hostStrToS(String)") goto "State2"
in("State4")
when("send_userAStrToS(String)") goto "State2"
in("State5")
when("send_acceptTStrToS(String)") goto "State2"
in("State6")
when("send_acceptLStrToS(String)") goto "State2"
in("State7")
when("send_acceptEStrToS(String)") goto "State2"
in("State8")
when("send_DNTIntToS(Integer)") goto "State2"
in("State9")
when("send_connectionStrToS(String)") goto "State2"
in("State10")
when("send_upgradeIRStrToS(String)") goto "State2"
in("State11")
when("send_cookieStrToS(String)") goto "State2"
in("State12")
when("send_bodyStrToS(String)") goto "State13"
in("State13")
when("receive_httpvStrFromS()") goto "State14"
in("State14")
when("receive_Choice1LabelFromS()") goto
"State15" at "_200" or
"State16" at "_404" 
in("State15")
when("receive_200StrFromS()") goto "State17"
in("State16")
when("receive_404StrFromS()") goto "State17"
in("State17")
when("receive_Choice2LabelFromS()") goto
"State18" at "DATE" or
"State19" at "SERVER" or
"State20" at "TRICTTS" or
"State21" at "LASTM" or
"State22" at "TAG" or
"State23" at "ACCEPTR" or
"State24" at "ONTENTL" or
"State25" at "VARY" or
"State26" at "ONTENTT" or
"State27" at "VIA" or
"State28" at "ACHEC" or
"State29" at "P3P" or
"State30" at "XSSPROTECTION" or
"State31" at "XFRAMEOPT" or
"State32" at "ETCOOKIE" or
"State33" at "TRANSFERE" or
"State34" at "XPIRES" or
"State35" at "BODY" 
in("State18")
when("receive_dateStrFromS()") goto "State17"
in("State19")
when("receive_serverStrFromS()") goto "State17"
in("State20")
when("receive_strictTSStrFromS()") goto "State17"
in("State21")
when("receive_lastMStrFromS()") goto "State17"
in("State22")
when("receive_eTagStrFromS()") goto "State17"
in("State23")
when("receive_acceptRStrFromS()") goto "State17"
in("State24")
when("receive_contentLIntFromS()") goto "State17"
in("State25")
when("receive_varyStrFromS()") goto "State17"
in("State26")
when("receive_contentTStrFromS()") goto "State17"
in("State27")
when("receive_viaStrFromS()") goto "State17"
in("State28")
when("receive_cacheCStrFromS()") goto "State17"
in("State29")
when("receive_p3pStrFromS()") goto "State17"
in("State30")
when("receive_xxssProtectionStrFromS()") goto "State17"
in("State31")
when("receive_xframeOptStrFromS()") goto "State17"
in("State32")
when("receive_setCookieStrFromS()") goto "State17"
in("State33")
when("receive_transferEStrFromS()") goto "State17"
in("State34")
when("receive_expiresStrFromS()") goto "State17"
in("State35")
when("receive_bodyStrFromS()") goto "end"
in("end")
end()
}