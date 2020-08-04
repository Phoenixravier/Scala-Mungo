echo "running test"

IF NOT EXIST protocolDir mkdir protocolDir
cd protocolDir
mkdir src
cd src
mkdir main
cd main
mkdir scala
cd scala
mkdir ProtocolDSL
cd..\..
mkdir test
cd test
mkdir scala
cd..\..
mkdir lib project target

echo name := "MyProject" > build.sbt
echo version := "1.0" >> build.sbt
echo scalaVersion := "2.13.3" >> build.sbt
echo libraryDependencies += "io.suzaku" %%%% "boopickle" %% "1.3.2" >> build.sbt
cd ..

call copy src\main\scala\ProtocolDSL\Example2.scala protocolDir\src\main\scala\ProtocolDSL
call copy src\main\scala\ProtocolDSL\ProtocolLang.scala protocolDir\src\main\scala\ProtocolDSL
call copy src\main\scala\ProtocolDSL\State.scala protocolDir\src\main\scala\ProtocolDSL

cd protocolDir

call sbt run
echo "after sbt run"



