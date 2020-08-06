REM this is just to make things go a little faster when I am only editing the plugin
call scalac -d classes -classpath classes src\main\scala\compilerPlugin\GetFileFromAnnotation.scala

cd classes
jar cf ..\jarName .
cd..

call scalac -Xplugin:jarName src\main\scala\compilerPlugin\Cat.scala