import ProtocolDSL.ProtocolLang
object StateIteratorProtocol extends ProtocolLang with App { 
in("init")
when("hasNext()") goto
"Next" at "true" or
"end" at "false" 
in("Next")
when("next()") goto "Remove"
in("Remove")
when("remove()") goto "init"
when("hasNext()") goto
"NextRemove" at "true" or
"end" at "false" 
in("NextRemove")
when("remove()") goto "Next"
when("next()") goto "Remove"
in("end")
end()
}