echo "hello, runnning test"
IF NOT EXIST testDir mkdir testDir

call copy src\main\scala\ProtocolDSL\Example.scala testDir\ProtocolDSL
call scalac -d testDir src\main\scala\ProtocolDSL\ProtocolLang.scala

cd testDir
jar cf ..\testJar .
cd..

call scalac testJar