import scala.reflect.runtime.universe._

val expr = reify{@Typestate(filename = "MyProtocol.txt")
class Cat{
}}

showRaw(expr.tree)