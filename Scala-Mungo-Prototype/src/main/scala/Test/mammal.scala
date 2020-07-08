package Test

import scala.reflect.runtime.{universe => u}

// Create Annotation `Mammal`
class mammal(indigenous:String) extends scala.annotation.StaticAnnotation

// Annotate class Platypus as a `Mammal`
@mammal(indigenous = "North America")
class Platypus{
  println(this)
}

object Main extends App {
  val Platy = new Platypus()
  println(Platy)
  val platypusType = u.typeOf[Platypus]
  val platypusSymbol = platypusType.typeSymbol.asClass
  print(platypusSymbol.annotations)
}


