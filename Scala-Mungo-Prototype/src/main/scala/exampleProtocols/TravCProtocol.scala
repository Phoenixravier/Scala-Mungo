package exampleProtocols

import ProtocolDSL.ProtocolLang
object TravCProtocol extends ProtocolLang with App {
in("init")
when("choiceFromA()") goto
"StateAfterChoiceADATA" at "DATA" or
"ChoiceB" at "NO_D" or
"end" at "END"
in("StateAfterChoiceADATA")
when("nodeFromA()") goto "ChoiceB"
in("ChoiceB")
when("choiceFromB()") goto
"StateAfterChoiceBDATA" at "DATA" or
"ChoiceC" at "NO_D" or
"ChoiceC" at "END"
in("StateAfterChoiceBDATA")
when("nodeFromB()") goto "ChoiceC"
in("ChoiceC")
when("DATAToA()") goto "StateAfterChoiceCDATAToA"
when("NO_DToA()") goto "StateDataToB"
when("ENDToA()") goto "StateDataToB"
in("StateAfterChoiceCDATAToA")
when("nodeToA(Node)") goto "StateDataToB"
in("StateDataToB")
when("DATAToB()") goto "StateAfterStateDataToB"
when("NO_DToB()") goto "init"
in("StateAfterStateDataToB")
when("nodeToB(Node)") goto "init"
in("end")
end()
}