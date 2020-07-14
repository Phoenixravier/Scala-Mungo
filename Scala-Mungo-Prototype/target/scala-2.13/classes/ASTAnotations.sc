import scala.reflect.runtime.universe._

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

val expr = reify{@Typestate(filename = "MyProtocol.txt")
class Cat{
}}

showRaw(expr.tree)

val tree = expr.tree

object traverser extends Traverser{
      var applies = List[Apply]()
      override def traverse(tree: Tree): Unit = tree match {
      case app @ Apply(fun, args) =>
      applies = app :: applies
      super.traverse(fun)
      super.traverseTrees(args)
      case _ => super.traverse(tree)
      }
}

traverser.traverse(tree)

println(traverser.applies)





