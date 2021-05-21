package compilerPlugin
//class Typestate(filename:String) extends scala.annotation.StaticAnnotation

@Typestate(filename = "emailProtocol")
class EmailClient{
  def start(){}
  def authenticate(){}
  def checkEmail(){}
  def logout(){}
  def stop(){}
}

object Main2 extends App{
  val emailClient = new EmailClient
  emailClient.start()
  emailClient.checkEmail()
  emailClient.stop()
}




