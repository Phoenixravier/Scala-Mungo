IF NOT EXIST protocolClasses mkdir protocolClasses
IF NOT EXIST protocolClasses/ProtocolDSL call scalac -d protocolClasses src/main/scala/ProtocolDSL/ProtocolLangClasses.scala
IF NOT EXIST protocolClasses/ProtocolDSL/ProtocolLang.class call scalac -classpath protocolClasses -d protocolClasses src/main/scala/ProtocolDSL/ProtocolLang.scala
call scalac -classpath protocolClasses -d protocolClasses %1
cd protocolClasses
SET difference=1
echo %difference%
IF EXIST ProtocolDSL/OldProtocol.class fc ProtocolDSL\OldProtocol.class ProtocolDSL/%2.class
IF EXIST ProtocolDSL/OldProtocol.class SET difference=%errorlevel%
echo %difference%
IF NOT %difference% EQU 0 del EncodedData.ser
IF NOT %difference% EQU 0 call scala -classpath . ProtocolDSL.%2
IF EXIST ProtocolDSL\OldProtocol.class del ProtocolDSL\OldProtocol.class
RENAME ProtocolDSL\%2.class OldProtocol.class
del ProtocolDSL\%2$.class
del ProtocolDSL\%2$delayedInit$body.class
IF EXIST EncodedData.ser echo encoded present
cd..


