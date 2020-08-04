IF NOT EXIST testDir mkdir testDir

call copy Example.scala testDir\src\main\scala\ProtocolDSL\
call copy ProtocolLang.scala testDir\src\main\scala\ProtocolDSL

cd testDir
sbt run

