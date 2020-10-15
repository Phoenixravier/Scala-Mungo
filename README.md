# Scala-Mungo
Fifth year six month MEng project at the University of Glasgow. Implementation of a typestate checking plugin for Scala.


Developper instructions:
Clone the project
Go into Scala-Mungo-Prototype
To run the plugin write "makejar" on the command line. 
This will by default run the Animals.scala file but this can be changed by editing the 
  call scalac -Xplugin:Scala-Mungo-Prototype_2.13.jar src\main\scala\compilerPlugin\Animals.scala
line to take a different file.

Building (which doesn't work at the moment):
run sbt publishLocal
Then the plugin should be available in your computer in a different project by adding the line:
  addCompilerPlugin("org.me" %% "GetFileFromAnnotation" % "0.2-SNAPSHOT")
to the build.sbt file.
