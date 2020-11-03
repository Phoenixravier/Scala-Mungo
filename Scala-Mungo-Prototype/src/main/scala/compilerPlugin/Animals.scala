package compilerPlugin

class Typestate(filename:String) extends scala.annotation.StaticAnnotation

import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
import java.net.{Socket, UnknownHostException}


object Choice1 extends Enumeration {
  type Choice1 = Value
  val APPROVE, REFUSE = Value
}

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.PrintWriter
import java.net.ServerSocket
import java.net.Socket
import java.net.UnknownHostException


@Typestate("src\\main\\scala\\exampleProtocols\\AProtocol.scala") class ARole() { // Bind the sockets
  private var socketRIn:BufferedReader = null
  private var socketROut:PrintWriter = null
  private var socketFIn:BufferedReader = null
  private var socketFOut:PrintWriter = null
  var serverF: ServerSocket = null
  try // Create the sockets
    serverF = new ServerSocket(20002)
  catch {
    case e: IOException =>
      System.out.println("Unable to listen on port")
      System.exit(-1)
  }
  // Accept a client connection
  var socketF: Socket = null
  try {
    System.out.println("Accepting...")
    socketF = serverF.accept
  } catch {
    case e: IOException =>
      System.out.println("Accept failed")
      System.exit(-1)
  }
  // Create the Finance read and write streams
  try {
    socketFIn = new BufferedReader(new InputStreamReader(socketF.getInputStream))
    socketFOut = new PrintWriter(socketF.getOutputStream, true)
  } catch {
    case e: IOException =>
      System.out.println("Read failed")
      System.exit(-1)
  }
  System.out.println("Accepted connection")
  // Connect to the servers
  try {
    val socketR = new Socket("localhost", 20001)
    // Create the Researcher read and write streams
    socketRIn = new BufferedReader(new InputStreamReader(socketR.getInputStream))
    socketROut = new PrintWriter(socketR.getOutputStream, true)
  } catch {
    case e: UnknownHostException =>
      System.out.println("Unable to connect to the remote host")
      System.exit(-1)
    case e: IOException =>
      System.out.println("Input/output error, unable to connect")
      System.exit(-1)
  }


  def receive_requestStringFromR(): String = {
    var line = ""
    try line = this.socketRIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error.")
        System.exit(-1)
    }
    line
  }

  def send_quoteintToR(payload: Int): Unit = {
    this.socketROut.println(payload)
  }

  def receive_Choice1LabelFromF(): Choice1.Value = {
    var stringLabelChoice1 = ""
    try stringLabelChoice1 = this.socketFIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error, unable to get label")
        System.exit(-1)
    }
    var intLabelChoice1 = 0
    if (stringLabelChoice1 == "APPROVE") intLabelChoice1 = 1
    else if (stringLabelChoice1 == "REFUSE") intLabelChoice1 = 2
    intLabelChoice1 match {
      case 1 =>
        Choice1.APPROVE
      case 2 =>
        Choice1.REFUSE
      case _ =>
        Choice1.REFUSE
    }
  }

  def receive_approveintFromF(): Int = {
    var line = ""
    try line = this.socketFIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error.")
        System.exit(-1)
    }
    line.toInt
  }

  def send_ticketStringToR(payload: String): Unit = {
    this.socketROut.println(payload)
  }

  def send_invoiceintToF(payload: Int): Unit = {
    this.socketFOut.println(payload)
  }

  def receive_paymentintFromF(): Int = {
    var line = ""
    try line = this.socketFIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error.")
        System.exit(-1)
    }
    line.toInt
  }

  def receive_refuseStringFromF(): String = {
    var line = ""
    try line = this.socketFIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error.")
        System.exit(-1)
    }
    line
  }
}

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader


object AMain {
  def safeRead(readerA: BufferedReader): String = {
    var readline = ""
    try readline = readerA.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error, unable to read")
        System.exit(-1)
    }
    readline
  }

  def main(args: Array[String]): Unit = { // Create the current role
    val currentA = new ARole
    // readerA can be used to input strings, and then use them in send method invocation
    val readerA = new BufferedReader(new InputStreamReader(System.in))
    // Method invocation follows the A typestate
    val payload1 = currentA.receive_requestStringFromR
    System.out.println("Received travel destination request from Researcher: " + payload1)
    System.out.print("Send quote price to Researcher: £")
    val payload2 = safeRead(readerA).toInt
    currentA.send_quoteintToR(payload2)
    currentA.receive_Choice1LabelFromF match {
      case Choice1.APPROVE =>
        val payload3 = currentA.receive_approveintFromF
        System.out.println("Received approval code from Finance: " + payload3)
        System.out.print("Send ticket to Researcher: ")
        val payload4 = safeRead(readerA)
        currentA.send_ticketStringToR(payload4)
        System.out.print("Send invoice code to Finance: ")
        val payload5 = safeRead(readerA).toInt
        currentA.send_invoiceintToF(payload5)
        val payload6 = currentA.receive_paymentintFromF
        System.out.println("Received payment from Finance: £" + payload6)
        System.out.println("\n	----TRANSACTION COMPLETE----	")

      case Choice1.REFUSE =>
        val payload7 = currentA.receive_refuseStringFromF
        System.out.println("Received refusal from Finance: " + payload7)
        System.out.println("\n	----TRANSACTION COMPLETE----	")

    }
  }
}





