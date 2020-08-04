import boopickle.Default._
import boopickle.UnpickleImpl

case class Person(name: String, age: Int)

object PicklingTest extends App {
  val data = Seq("Hello", "World!")
  val buf = Pickle.intoBytes(data)
  println(buf)
  val helloWorld = UnpickleImpl[Seq[String]].fromBytes(buf)
  println(helloWorld)
}