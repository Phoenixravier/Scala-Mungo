@echo on

rem %1 is the path to the protocol file
rem %2 is the name of the class

IF NOT EXIST protocolClasses mkdir protocolClasses
IF NOT EXIST protocolClasses/ProtocolDSL call scalac -d protocolClasses src/main/scala/ProtocolDSL/ProtocolLangClasses.scala
IF NOT EXIST protocolClasses/ProtocolDSL/ProtocolLang.class call scalac -classpath protocolClasses -d protocolClasses src/main/scala/ProtocolDSL/ProtocolLang.scala
call scalac -classpath protocolClasses -d protocolClasses %1
cd protocolClasses
del EncodedData.ser
IF EXIST ProtocolDSL/%2.class (
    echo executing protocolDsl\class
    call scala -classpath . ProtocolDSL.%2
    del ProtocolDSL\%2.class
    del ProtocolDSL\%2$.class
    del ProtocolDSL\%2$delayedInit$body.class
    ) ELSE (
        echo executing class
        call scala -classpath . %2
        del %2.class
        del %2$.class
        del %2$delayedInit$body.class
        )
IF EXIST EncodedData.ser echo encoded present
cd..


