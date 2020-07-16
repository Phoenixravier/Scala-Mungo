call scalac -d classes src\main\scala\compilerPlugin\GetFileFromAnnotation.scala

cd classes
jar cf ..\getfile.jar .
cd..
call scalac -Xplugin:getfile.jar src\main\scala\compilerPlugin\Cat.scala

