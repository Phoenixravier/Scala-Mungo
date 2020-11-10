package Dogs

import compilerPlugin.Typestate

@Typestate("DogProtocol")
class Cat {
  def walk():Unit = println("Jee kävelemme!")
  def cry():Unit = println("Itkeen :'(")
  def bark():Unit = println("hau hau")
  def laze():Unit = println("Olen väsynyt")
  def stayOnAlert(intruderHere:Boolean): Unit = {
    if(intruderHere) bark()
    else laze()
  }
  def stayOnAlert(str:String, nb:Int): Unit ={
    println("on alert")
  }
}
