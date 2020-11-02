import ProtocolDSL.ProtocolLang
object StateIteratorProtocol extends ProtocolLang with App { 
in("init")
when("hasNext()") goto
"Next" at "TRUE" or
"end" at "FALSE" 
in("Next")
when("next()") goto "Remove"
in("Remove")
when("remove()") goto "init"
when("hasNext()") goto
"NextRemove" at "TRUE" or
"end" at "FALSE" 
in("NextRemove")
when("remove()") goto "Next"
when("next()") goto "Remove"
in("end")
end()
}