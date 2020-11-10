package exampleProtocols

import ProtocolDSL.ProtocolLang
object TravAProtocol extends ProtocolLang with App {
in("init")
when("DATAToB()") goto "StateAfterDATAToB"
when("NO_DToB()") goto "StateDataToC"
when("ENDToB()") goto "StateAfterENDToB"
in("StateAfterENDToB")
when("ENDToC()") goto "end"
in("StateAfterDATAToB")
when("nodeToB(Node)") goto "StateDataToC"
in("StateDataToC")
when("DATAToC()") goto "StateAfterDATAToC"
when("NO_DToC()") goto "ChoiceB"
in("StateAfterDATAToC")
when("nodeToC(Node)") goto "ChoiceB"
in("ChoiceB")
when("choiceFromB()") goto
"StateAfterChoiceBDATA" at "DATA" or
"ChoiceC" at "NO_D" or
"ChoiceC" at "END"
in("StateAfterChoiceBDATA")
when("nodeFromB()") goto "ChoiceC"
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