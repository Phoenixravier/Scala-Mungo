import ProtocolDSL.ProtocolLang
object Loop extends ProtocolLang with App { 
in("init")
when("finished()") goto
"init" at "FALSE" or
"end" at "TRUE" 
in("end")
end()
}