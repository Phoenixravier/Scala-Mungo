jar:
	mkdir -p classes
	cp src/main/scala/scalac-plugin.xml classes
	scalac -d classes -classpath classes \
		src/main/scala/ProtocolDSL/ProtocolLangClasses.scala \
		src/main/scala/ProtocolDSL/ProtocolLang.scala \
		src/main/scala/compilerPlugin/PluginClasses.scala \
		src/main/scala/compilerPlugin/Util.scala \
		src/main/scala/compilerPlugin/GetFileFromAnnotation.scala \
        src/main/scala/compilerPlugin/Typestate.scala
	jar cf Scala-Mungo-Prototype_2.13.jar -C classes .


