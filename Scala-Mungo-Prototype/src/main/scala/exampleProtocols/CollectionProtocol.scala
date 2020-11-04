import ProtocolDSL.ProtocolLang
object CollectionProtocol extends ProtocolLang with App { 
in("init")
when("initialise(Int)") goto "Empty"
in("Empty")
when("put(Node)") goto "NonEmpty"
when("close()") goto "end"
in("NonEmpty")
when("put(Node)") goto "NonEmpty"
when("get()") goto "Unknown"
in("Unknown")
when("put(Node)") goto "NonEmpty"
when("isEmpty()") goto
"Empty" at "true" or
"NonEmpty" at "false"
in("end")
end()
}