# Scala-Mungo
## Fifth year six month MEng project at the University of Glasgow. Implementation of a typestate checking plugin for Scala.

## Don't use this yet! It isn't finished

### Tutorial
View tutorial at https://aliceravier.github.io/

### Getting the tool
Warnings:
- only works on Windows unless you change the batch file to work on your computer.
- make sure you do not have a directory called "protocolClasses" in the root of your project folder. If you do, this plugin will create new files inside it and might not work.
- this assumes that you have installed coursier in the default folder and will look for: C:\Users\%username%\AppData\Local\Coursier\cache\v1 to find the scala-mungo jar. If this is not the case, after STEP 1, you must go into the "executeUserProtocol.bat" file and change the 
- only works if the protocol file is either: not in a package OR in the ProtocolDSL package.
```
call scalac -cp "C:\Users\%username%\AppData\Local\Coursier\cache\v1\https\dl.bintray.com\aliceravier\maven\default\scala-mungo-prototype_2.13\1.0\scala-mungo-prototype_2.13-1.0.jar" -d protocolClasses %1
```
line. The string after "-cp" must be changed to the location of the scala-mungo-prototype_2.13-1.0.jar file on your computer
Same for the line:
```
call scala -cp ".;C:\Users\%username%\AppData\Local\Coursier\cache\v1\https\dl.bintray.com\aliceravier\maven\default\scala-mungo-prototype_2.13\1.0\scala-mungo-prototype_2.13-1.0.jar" %2
```
where the string after "-cp" must be changed to ".;yourPath".



STEP 1:
Add these lines to your build.sbt:
```
resolvers += Resolver.bintrayRepo("aliceravier", "maven")
autoCompilerPlugins := true
addCompilerPlugin("default" %% "scala-mungo-prototype" % "1.0")
libraryDependencies += "default" %% "scala-mungo-prototype" % "1.0"
```
STEP 2:
From https://bintray.com/aliceravier/maven/scala-mungo-prototype/1.0?sort=&order=#files, copy the file  "executeUserProtocol.bat" into the root level of your project directory.


### Developper instructions:
* Clone the project
* Go into Scala-Mungo-Prototype by using
```
cd Scala-Mungo-Prototype
```
To run the plugin use:
```
$ makejar 
```
This will by default run the Animals.scala file but this can be changed by editing the 
```
call scalac -Xplugin:Scala-Mungo-Prototype_2.13.jar src\main\scala\compilerPlugin\Animals.scala
```
line to take a different file.

### Building:
run 
```
$ sbt publishLocal
```
Then the plugin should be available on your computer in a different project by adding the line:
```
addCompilerPlugin("org.me" %% "GetFileFromAnnotation" % "0.2-SNAPSHOT")
```
to the build.sbt file.

