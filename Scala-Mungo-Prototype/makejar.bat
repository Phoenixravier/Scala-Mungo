set jarName = Scala-Mungo-Prototype_2.13.jar

mkdir classes
copy src\main\scala\compilerPlugin\scalac-plugin.xml classes
call scalac -d classes src\main\scala\ProtocolDSL\ProtocolLangClasses.scala
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\GetFileFromAnnotation.scala

cd classes
jar cf ..\jarName .
cd..

call scalac -Xplugin:jarName src\main\scala\compilerPlugin\Cat.scala

