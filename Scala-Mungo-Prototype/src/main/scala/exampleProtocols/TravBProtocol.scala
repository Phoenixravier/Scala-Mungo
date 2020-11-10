package exampleProtocols

import ProtocolDSL.ProtocolLang
object TravBProtocol extends ProtocolLang with App { 
in("init")
when("choiceFromA()") goto
"StateAfterChoiceADATA" at "DATA" or
"ChoiceB" at "NO_D" or
"end" at "END"
in("StateAfterChoiceADATA")
when("nodeFromA()") goto "ChoiceB"
in("ChoiceB")
when("DATAToA()") goto "StateAfterChoiceBDATA"
when("NO_DToA()") goto "StateDataToC"
when("ENDToA()") goto "StateDataToC"
in("StateAfterChoiceBDATA")
when("nodeToA(Node)") goto "StateDataToC"
in("StateDataToC")
when("DATAToC()") goto "StateAfterDataToCDATAToC"
when("NO_DToC()") goto "ChoiceC"
in("StateAfterDataToCDATAToC")
when("nodeToC(Node)") goto "ChoiceC"
in("ChoiceC")
when("choiceFromC()") goto
"StateAfterChoiceCDATA" at "DATA" or
"init" at "NO_D" or
"init" at "END"
in("StateAfterChoiceCDATA")
when("nodeFromC()") goto "init"
in("end")
end()
}