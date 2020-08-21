@echo off
REM creates the directory structure for an sbt project
IF NOT EXIST protocolDir mkdir protocolDir
cd protocolDir
IF NOT EXIST src mkdir src
cd src
IF NOT EXIST main mkdir main
cd main
IF NOT EXIST scala mkdir scala
cd scala
IF NOT EXIST ProtocolDSL mkdir ProtocolDSL
cd..\..
IF NOT EXIST test mkdir test
cd test
IF NOT EXIST scala mkdir scala
cd..\..
IF NOT EXIST lib mkdir lib
IF NOT EXIST project mkdir project
IF NOT EXIST target mkdir target

echo name := "MyProject" > build.sbt
echo version := "1.0" >> build.sbt
echo scalaVersion := "2.13.3" >> build.sbt
cd ..

call copy %1 protocolDir\src\main\scala\ProtocolDSL
call copy src\main\scala\ProtocolDSL\ProtocolLang.scala protocolDir\src\main\scala\ProtocolDSL
call copy src\main\scala\ProtocolDSL\ProtocolLangClasses.scala protocolDir\src\main\scala\ProtocolDSL

cd protocolDir

call sbt run

cd ..
@RD /S /Q "protocolDir\src"



