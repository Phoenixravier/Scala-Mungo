@echo on

IF NOT EXIST classes mkdir classes

copy src\main\scala\compilerPlugin\scalac-plugin.xml classes
call scalac -d classes src\main\scala\ProtocolDSL\ProtocolLangClasses.scala
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\GetFileFromAnnotation.scala

cd classes
jar cf ..\Scala-Mungo-Prototype_2.13.jar .
cd..

call scalac -Xplugin:Scala-Mungo-Prototype_2.13.jar src\main\scala\compilerPlugin\Animals.scala


