@echo on

IF NOT EXIST classes mkdir classes

copy src\main\scala\scalac-plugin.xml classes
IF NOT EXIST classes\ProtocolDSL call scalac -d classes src\main\scala\ProtocolDSL\ProtocolLangClasses.scala
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\PluginClasses.scala
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\Util.scala
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\GetFileFromAnnotation.scala

cd classes
jar cf ..\Scala-Mungo-Prototype_2.13.jar .
cd..

call scalac -Xplugin:Scala-Mungo-Prototype_2.13.jar src\main\scala\compilerPlugin\Animals.scala

REM del "Scala-Mungo-Prototype_2.13.jar"


