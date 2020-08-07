import compilerPlugin.Cat

class Cat {
  println("hello from cat")
}

object Dog{
  def dosthg(): Unit ={
    println("fasf")
  }
}
object execute extends App{
  println("hi")
  val cat = new compilerPlugin.Cat()
  Dog.dosthg()
}
