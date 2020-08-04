set jarName = Scala-Mungo-Prototype_2.13.jar

call scalac -d classes src\main\scala\ProtocolDSL\State.scala
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\GetFileFromAnnotation.scala

cd classes
jar cf ..\jarName .
cd..

call scalac -Xplugin:jarName src\main\scala\compilerPlugin\Cat.scala

