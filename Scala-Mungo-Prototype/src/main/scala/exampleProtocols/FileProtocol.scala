import ProtocolDSL.ProtocolLang
object FileProtocol extends ProtocolLang with App { 
in("init")
when("open()") goto
"Open" at "OK" or
"end" at "ERROR" 
in("Open")
when("hasNext()") goto
"Read" at "true" or
"Close" at "false"
when("close()") goto "end"
in("Read")
when("read()") goto "Open"
in("Close")
when("close()") goto "end"
in("end")
end()
}