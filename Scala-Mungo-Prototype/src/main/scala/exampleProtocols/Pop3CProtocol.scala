import ProtocolDSL.ProtocolLang
object Pop3CProtocol extends ProtocolLang with App { 
in("init")
when("receive_OKNStringFromS()") goto "State1"
in("State1")
when("send_USERToS()") goto "State2"
when("send_QUITToS()") goto "State62"
in("State2")
when("send_USERStringToS(String)") goto "State3"
in("State3")
when("receive_Choice1LabelFromS()") goto
"State4" at "OK" or
"State61" at "ERR" 
in("State4")
when("receive_OKStringFromS()") goto "State5"
in("State5")
when("send_PASSToS()") goto "State6"
when("send_QUITToS()") goto "State59"
in("State6")
when("send_PASSStringToS(String)") goto "State7"
in("State7")
when("receive_Choice1LabelFromS()") goto
"State8" at "OK" or
"State58" at "ERR" 
in("State8")
when("receive_OKStringFromS()") goto "State9"
in("State9")
when("send_STATToS()") goto "State10"
when("send_LISTToS()") goto "State12"
when("send_LIST_NToS()") goto "State19"
when("send_RETR_NToS()") goto "State23"
when("send_DELE_NToS()") goto "State30"
when("send_RSETToS()") goto "State34"
when("send_TOP_NToS()") goto "State36"
when("send_NOOPToS()") goto "State43"
when("send_QUITToS()") goto "State45"
when("send_UIDLToS()") goto "State47"
when("send_UIDL_NToS()") goto "State54"
in("State10")
when("send_STATVoidToS(Void)") goto "State11"
in("State11")
when("receive_OKNTwoIntFromS()") goto "State9"
in("State12")
when("send_LISTVoidToS(Void)") goto "State13"
in("State13")
when("receive_Choice1LabelFromS()") goto
"State14" at "OK" or
"State18" at "ERR" 
in("State14")
when("receive_OKStringFromS()") goto "State15"
in("State15")
when("receive_Choice2LabelFromS()") goto
"State16" at "DOT" or
"State17" at "SUM" 
in("State16")
when("receive_DOTVoidFromS()") goto "State9"
in("State17")
when("receive_SUMTwoIntFromS()") goto "State15"
in("State18")
when("receive_ERRStringFromS()") goto "State9"
in("State19")
when("send_LIST_nIntToS(int)") goto "State20"
in("State20")
when("receive_Choice1LabelFromS()") goto
"State21" at "OK" or
"State22" at "ERR" 
in("State21")
when("receive_OKTwoIntFromS()") goto "State9"
in("State22")
when("receive_ERRStringFromS()") goto "State9"
in("State23")
when("send_RETR_nIntToS(int)") goto "State24"
in("State24")
when("receive_Choice1LabelFromS()") goto
"State25" at "OK" or
"State29" at "ERR" 
in("State25")
when("receive_OKStringFromS()") goto "State26"
in("State26")
when("receive_Choice2LabelFromS()") goto
"State27" at "DOT" or
"State28" at "SUM" 
in("State27")
when("receive_DOTVoidFromS()") goto "State9"
in("State28")
when("receive_SUMStringFromS()") goto "State26"
in("State29")
when("receive_ERRStringFromS()") goto "State9"
in("State30")
when("send_DELE_nIntToS(int)") goto "State31"
in("State31")
when("receive_Choice1LabelFromS()") goto
"State32" at "OK" or
"State33" at "ERR" 
in("State32")
when("receive_OKStringFromS()") goto "State9"
in("State33")
when("receive_ERRStringFromS()") goto "State9"
in("State34")
when("send_RSETVoidToS(Void)") goto "State35"
in("State35")
when("receive_OKNStringFromS()") goto "State9"
in("State36")
when("send_TOP_nTwoIntToS(TwoInt)") goto "State37"
in("State37")
when("receive_Choice1LabelFromS()") goto
"State38" at "OK" or
"State42" at "ERR" 
in("State38")
when("receive_OKStringFromS()") goto "State39"
in("State39")
when("receive_Choice2LabelFromS()") goto
"State40" at "DOT" or
"State41" at "SUM" 
in("State40")
when("receive_DOTVoidFromS()") goto "State9"
in("State41")
when("receive_SUMStringFromS()") goto "State39"
in("State42")
when("receive_ERRStringFromS()") goto "State9"
in("State43")
when("send_NOOPVoidToS(Void)") goto "State44"
in("State44")
when("receive_OKNVoidFromS()") goto "State9"
in("State45")
when("send_QUITVoidToS(Void)") goto "State46"
in("State46")
when("receive_OKNStringFromS()") goto "end"
in("State47")
when("send_UIDLVoidToS(Void)") goto "State48"
in("State48")
when("receive_Choice1LabelFromS()") goto
"State49" at "OK" or
"State53" at "ERR" 
in("State49")
when("receive_OKStringFromS()") goto "State50"
in("State50")
when("receive_Choice2LabelFromS()") goto
"State51" at "DOT" or
"State52" at "SUM" 
in("State51")
when("receive_DOTVoidFromS()") goto "State9"
in("State52")
when("receive_SUMIntStringFromS()") goto "State50"
in("State53")
when("receive_ERRStringFromS()") goto "State9"
in("State54")
when("send_UIDL_nIntToS(int)") goto "State55"
in("State55")
when("receive_Choice1LabelFromS()") goto
"State56" at "OK" or
"State57" at "ERR" 
in("State56")
when("receive_OKIntStringFromS()") goto "State9"
in("State57")
when("receive_ERRStringFromS()") goto "State9"
in("State58")
when("receive_ERRStringFromS()") goto "State5"
in("State59")
when("send_QUITVoidToS(Void)") goto "State60"
in("State60")
when("receive_OKNStringFromS()") goto "end"
in("State61")
when("receive_ERRStringFromS()") goto "State1"
in("State62")
when("send_QUITVoidToS(Void)") goto "State63"
in("State63")
when("receive_OKNStringFromS()") goto "end"
in("end")
end()
}