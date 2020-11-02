package compilerPlugin

import scala.tools.nsc.doc.html.HtmlTags.I
import scala.util.control.Breaks
import scala.util.control.Breaks.{break, breakable}

class Typestate(filename:String) extends scala.annotation.StaticAnnotation
/*
@Typestate(filename = "src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
class Cat{

  def go() = ???
  def grab() = ???
  def stop() = ???
  def jump() = ???

  println("making a cat")
  def comeAlive() = println("The cat is alive")
  def walk(): Boolean = {
    comeAlive()
    true
  }
  def m() ={
  }
}

 */

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.PrintWriter
import java.net.ServerSocket
import java.net.Socket


@Typestate("src\\main\\scala\\exampleProtocols\\AdderCProtocol.scala") class CRole() { // Bind the sockets
  var serverS: ServerSocket = null
  // Connecting to the server
  try // Create the sockets
    serverS = new ServerSocket(20000)
  catch {
    case e: IOException =>
      System.out.println("Unable to listen on ports")
      System.exit(-1)
  }
  // Accept a client connection
  var socketS: Socket = null
  try {
    System.out.println("Accepting...")
    socketS = serverS.accept
    System.out.println("S accepted")
  } catch {
    case e: IOException =>
      System.out.println("Accept failed")
      System.exit(-1)
  }
  private var socketSIn:BufferedReader = null
  private var socketSOut:PrintWriter = null
  // Create the read and write streams
  try {
    socketSIn = new BufferedReader(new InputStreamReader(socketS.getInputStream))
    socketSOut = new PrintWriter(socketS.getOutputStream, true)
  } catch {
    case e: IOException =>
      System.out.println("Read failed")
      System.exit(-1)
  }

  def send_ADDToS(): Unit = {
    this.socketSOut.println("ADD")
  }

  def send_BYEToS(): Unit = {
    this.socketSOut.println("BYE")
  }

  def send_AddintToS(payload: Integer): Unit = {
    this.socketSOut.println(payload)
  }

  def receive_ResintFromS: Integer = {
    var line = ""
    try line = this.socketSIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Outpur error. " + e.getMessage)
        System.exit(-1)
    }
    line.toInt
  }

  def send_ByeToS(): Unit = {
    // Nothing to be sent
  }
}

object Main{

  import java.io.BufferedReader
  import java.io.IOException
  import java.io.InputStreamReader
    def safeRead(readerC: BufferedReader) = {
      var readline = ""
      try readline = readerC.readLine
      catch {
        case e: IOException =>
          System.out.println("Input/Output error, unable to read")
          System.exit(-1)
      }
      readline
    }

    def main(args: Array[String]): Unit = { // Create the current role
      val currentC = new CRole
      // readerC can be used to input strings, and then use them in send method invocation
      val readerC = new BufferedReader(new InputStreamReader(System.in))
      val Outer = new Breaks
      val Inner = new Breaks
      // Method invocation follows the C typestate
      Outer.breakable {
        do {
          Inner.breakable {
            System.out.print("Choose a label among [ADD, BYE]: ")
            val sread1 = safeRead(readerC)
            sread1 match {
              case "ADD" =>
                currentC.send_ADDToS()
                System.out.print("Send to S: ")
                val payload1 = safeRead(readerC).toInt
                currentC.send_AddintToS(payload1)
                System.out.print("Send to S: ")
                val payload2 = safeRead(readerC).toInt
                currentC.send_AddintToS(payload2)
                val payload3 = currentC.receive_ResintFromS
                System.out.println("Received from S: " + payload3)
                Inner.break()
              case "BYE" =>
                currentC.send_BYEToS()
                currentC.send_ByeToS()
                Outer.break()
            }
          }
        } while ( {
          true
        })
      }
    }
}




//import scala.language.postfixOps
/*
  //@Typestate(filename="src\\main\\scala\\ProtocolDSL\\DogProtocol.scala")
  object Dog extends Serializable{
    def walk():Unit = println("Jee kävelemme!")
    def cry():Unit = println("Itken :'(")
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


@Typestate(filename="src\\main\\scala\\ProtocolDSL\\CatProtocol.scala")
case class Cat(var id:Int=0){
  println("init "+id)
  def selfChange(kit:Cat): Unit ={
    kit.walk()
  }
  def comeAlive(s:String, i:Int):String = "alternative come alive"
  def comeAlive():Unit = println("The cat is alive")
  def run():Unit = println("Running")
  def rest():Unit = println("Resting")
  def walk():Boolean = {
    println("walking "+id)
    false
  }
  def sleep():Unit = println("Sleeping")


}
*/





