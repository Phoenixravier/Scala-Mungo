
import java.io.{BufferedWriter, File, FileWriter}

import ProtocolDSL.State
import compilerPlugin.{GetFileFromAnnotation, inconsistentStateMutation, protocolViolatedException}
import org.scalatest._

import scala.collection.SortedSet
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Settings, _}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter


class ExamplesTest extends FlatSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll{

  //region Adder
  "adder client" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.net.ServerSocket
        |import java.net.Socket
        |import scala.util.control.Breaks
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\AdderCProtocol.scala") class CRole() { // Bind the sockets
        |  var serverS: ServerSocket = null
        |  // Connecting to the server
        |  try // Create the sockets
        |    serverS = new ServerSocket(20000)
        |  catch {
        |    case e: IOException =>
        |      System.out.println("Unable to listen on ports")
        |      System.exit(-1)
        |  }
        |  // Accept a client connection
        |  var socketS: Socket = null
        |  try {
        |    System.out.println("Accepting...")
        |    socketS = serverS.accept
        |    System.out.println("S accepted")
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Accept failed")
        |      System.exit(-1)
        |  }
        |  private var socketSIn:BufferedReader = null
        |  private var socketSOut:PrintWriter = null
        |  // Create the read and write streams
        |  try {
        |    socketSIn = new BufferedReader(new InputStreamReader(socketS.getInputStream))
        |    socketSOut = new PrintWriter(socketS.getOutputStream, true)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Read failed")
        |      System.exit(-1)
        |  }
        |
        |  def send_ADDToS(): Unit = {
        |    this.socketSOut.println("ADD")
        |  }
        |
        |  def send_BYEToS(): Unit = {
        |    this.socketSOut.println("BYE")
        |  }
        |
        |  def send_AddintToS(payload: Integer): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def receive_ResintFromS(): Integer = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_ByeToS(): Unit = {
        |    // Nothing to be sent
        |  }
        |}
        |
        |object Main{
        |
        |  import java.io.BufferedReader
        |  import java.io.IOException
        |  import java.io.InputStreamReader
        |    def safeRead(readerC: BufferedReader) = {
        |      var readline = ""
        |      try readline = readerC.readLine
        |      catch {
        |        case e: IOException =>
        |          System.out.println("Input/Output error, unable to read")
        |          System.exit(-1)
        |      }
        |      readline
        |    }
        |
        |    def main(args: Array[String]): Unit = { // Create the current role
        |      val currentC = new CRole
        |      // readerC can be used to input strings, and then use them in send method invocation
        |      val readerC = new BufferedReader(new InputStreamReader(System.in))
        |      val Outer = Breaks
        |      val Inner = Breaks
        |      // Method invocation follows the C typestate
        |      Outer.breakable {
        |        do {
        |          Inner.breakable {
        |            System.out.print("Choose a label among [ADD, BYE]: ")
        |            val sread1 = safeRead(readerC)
        |            sread1 match {
        |              case "ADD" =>
        |                currentC.send_ADDToS()
        |                System.out.print("Send to S: ")
        |                val payload1 = safeRead(readerC).toInt
        |                currentC.send_AddintToS(payload1)
        |                System.out.print("Send to S: ")
        |                val payload2 = safeRead(readerC).toInt
        |                currentC.send_AddintToS(payload2)
        |                val payload3 = currentC.receive_ResintFromS()
        |                System.out.println("Received from S: " + payload3)
        |                Inner.break()
        |              case "BYE" =>
        |                currentC.send_BYEToS()
        |                currentC.send_ByeToS()
        |                Outer.break()
        |            }
        |          }
        |        } while (true)
        |      }
        |    }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "adder server" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |import scala.util.control.Breaks.{break, breakable}
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.net.Socket
        |import java.net.UnknownHostException
        |
        |object SChoice1 extends Enumeration {
        |  type SChoice1 = Value
        |  val ADD, BYE = Value
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\SProtocol.scala") class SRole() { // Connect to the other participants in the protocol
        |  private var socketCIn: BufferedReader = null
        |  private var socketCOut: PrintWriter = null
        |  try { // Create the sockets
        |    val socketC = new Socket("localhost", 20000)
        |    socketCIn = new BufferedReader(new InputStreamReader(socketC.getInputStream))
        |    socketCOut = new PrintWriter(socketC.getOutputStream, true)
        |  } catch {
        |    case e: UnknownHostException =>
        |      System.out.println("Unable to connect to the remote host")
        |      System.exit(-1)
        |    case e: IOException =>
        |      System.out.println("Input/output error")
        |      System.exit(-1)
        |  }
        |
        |
        |  def receive_SChoice1LabelFromC(): SChoice1.Value = {
        |    var stringLabelSChoice1 = ""
        |    try stringLabelSChoice1 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice1 match {
        |      case "ADD" =>
        |        SChoice1.ADD
        |      case "BYE" =>
        |        SChoice1.BYE
        |      case _ =>
        |        SChoice1.BYE
        |    }
        |  }
        |
        |  def receive_AddintFromC(): Integer = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_ResintToC(payload: Integer): Unit = {
        |    this.socketCOut.println(payload)
        |  }
        |
        |  def receive_ByeFromC(): Unit = {
        |    // Nothing to be received
        |  }
        |}
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |
        |
        |object SMain {
        |  def safeRead(readerS: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerS.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val currentS = new SRole
        |    // readerS can be used to input strings, and then use them in send method invocation
        |    val readerS = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the S typestate
        |    do
        |    breakable {
        |      currentS.receive_SChoice1LabelFromC() match {
        |        case SChoice1.ADD =>
        |          val payload1 = currentS.receive_AddintFromC()
        |          System.out.println("Received from C: " + payload1)
        |          val payload2 = currentS.receive_AddintFromC()
        |          System.out.println("Received from C: " + payload2)
        |          System.out.print("Send to C: ")
        |          val payload3 = safeRead(readerS).toInt
        |          currentS.send_ResintToC(payload3)
        |          break()
        |        case SChoice1.BYE =>
        |          currentS.receive_ByeFromC()
        |          break()
        |      }
        |    }while ( {
        |      true
        |    })
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region Bookstore
  "bookstore1" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
        |import java.net.{Socket, UnknownHostException}
        |
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val AGREE, QUIT = Value
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\Buyer1Protocol.scala") class Buyer1Role() { // Connect to the other participants in the protocol
        |  private var socketSellerIn:BufferedReader = _
        |  private var socketSellerOut:PrintWriter = _
        |  private var socketBuyer2In:BufferedReader = _
        |  private var socketBuyer2Out:PrintWriter = _
        |  try { // Create the sockets
        |    val socketSeller = new Socket("localhost", 20000)
        |    val socketBuyer2 = new Socket("localhost", 20002)
        |    socketSellerIn = new BufferedReader(new InputStreamReader(socketSeller.getInputStream))
        |    socketSellerOut = new PrintWriter(socketSeller.getOutputStream, true)
        |    socketBuyer2In = new BufferedReader(new InputStreamReader(socketBuyer2.getInputStream))
        |    socketBuyer2Out = new PrintWriter(socketBuyer2.getOutputStream, true)
        |  } catch {
        |    case e: UnknownHostException =>
        |      System.out.println("Unable to connect to the remote host")
        |      System.exit(-1)
        |    case e: IOException =>
        |      System.out.println("Input/output error")
        |      System.exit(-1)
        |  }
        |
        |
        |  def send_bookStringToSeller(payload: String): Unit = {
        |    this.socketSellerOut.println(payload)
        |  }
        |
        |  def receive_bookintFromSeller(): Int = {
        |    var line = ""
        |    try line = this.socketSellerIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_quoteintToBuyer2(payload: Int): Unit = {
        |    this.socketBuyer2Out.println(payload)
        |  }
        |
        |  def receive_Choice1LabelFromBuyer2(): Choice1.Value = {
        |    var stringLabelChoice1 = ""
        |    try stringLabelChoice1 = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to get label")
        |        System.exit(-1)
        |    }
        |    stringLabelChoice1 match {
        |      case "AGREE" =>
        |        System.out.println("Buyer2 Agrees")
        |        Choice1.AGREE
        |      case "QUIT" =>
        |        System.out.println("Buyer2 Refuses")
        |        Choice1.QUIT
        |      case _ =>
        |        Choice1.QUIT
        |    }
        |  }
        |
        |  def receive_agreeStringFromBuyer2(): String = {
        |    var line = ""
        |    try line = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_transferintToSeller(payload: Int): Unit = {
        |    this.socketSellerOut.println(payload)
        |  }
        |
        |  def receive_quitStringFromBuyer2(): String = {
        |    var line = ""
        |    try line = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |}
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader}
        |
        |
        |object Buyer1Main {
        |  def safeRead(readerBuyer1: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerBuyer1.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val currentBuyer1 = new Buyer1Role
        |    // readerBuyer1 can be used to input strings, and then use them in send method invocation
        |    val readerBuyer1 = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the Buyer1 typestate
        |    System.out.print("Send book title to Seller: ")
        |    val payload1 = safeRead(readerBuyer1)
        |    currentBuyer1.send_bookStringToSeller(payload1)
        |    val payload2 = currentBuyer1.receive_bookintFromSeller()
        |    System.out.println("Received book price from Seller: £" + payload2)
        |    System.out.print("Send book price quote to Buyer2: £")
        |    val payload3 = safeRead(readerBuyer1).toInt
        |    currentBuyer1.send_quoteintToBuyer2(payload3)
        |    currentBuyer1.receive_Choice1LabelFromBuyer2() match {
        |      case Choice1.AGREE =>
        |        val payload4 = currentBuyer1.receive_agreeStringFromBuyer2()
        |        System.out.println("Received agreement message from Buyer2: " + payload4)
        |        System.out.print("Send transfer to Seller: £")
        |        val payload5 = safeRead(readerBuyer1).toInt
        |        currentBuyer1.send_transferintToSeller(payload5)
        |        System.out.println("\n---Transaction complete: book purchased---")
        |
        |      case Choice1.QUIT =>
        |        val payload6 = currentBuyer1.receive_quitStringFromBuyer2()
        |        System.out.println("Received quit message from Buyer2: " + payload6)
        |        System.out.println("\n---Transaction complete: no sale---")
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "bookstore2" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val AGREE, QUIT = Value
        |}
        |
        |import compilerPlugin.Typestate
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.net.ServerSocket
        |import java.net.Socket
        |import java.net.UnknownHostException
        |
        |//import mungo.lib.Typestate;
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\Buyer2Protocol.scala") class Buyer2Role() { // Bind the sockets
        |  private var socketSellerIn:BufferedReader = null
        |  private var socketSellerOut:PrintWriter = null
        |  private var socketBuyer1In:BufferedReader = null
        |  private var socketBuyer1Out:PrintWriter = null
        |  var serverBuyer1: ServerSocket = null
        |  try // Create the sockets
        |    serverBuyer1 = new ServerSocket(20002)
        |  catch {
        |    case e: IOException =>
        |      System.out.println("Unable to listen on port")
        |      System.exit(-1)
        |  }
        |  // Accept a client connection
        |  var socketBuyer1: Socket = null
        |  try {
        |    System.out.println("Accepting...")
        |    socketBuyer1 = serverBuyer1.accept
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Accept failed")
        |      System.exit(-1)
        |  }
        |  // Create the read and write streams
        |  try {
        |    socketBuyer1In = new BufferedReader(new InputStreamReader(socketBuyer1.getInputStream))
        |    socketBuyer1Out = new PrintWriter(socketBuyer1.getOutputStream, true)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Read failed")
        |      System.exit(-1)
        |  }
        |  System.out.println("Accepted connection")
        |  // Connect to the servers
        |  try {
        |    val socketSeller = new Socket("localhost", 20001)
        |    socketSellerIn = new BufferedReader(new InputStreamReader(socketSeller.getInputStream))
        |    socketSellerOut = new PrintWriter(socketSeller.getOutputStream, true)
        |  } catch {
        |    case e: UnknownHostException =>
        |      System.out.println("Unable to connect to the remote host")
        |      System.exit(-1)
        |    case e: IOException =>
        |      System.out.println("Input/output error, unable to connect")
        |      System.exit(-1)
        |  }
        |
        |
        |  def receive_quoteintFromBuyer1(): Int = {
        |    var line = ""
        |    try line = this.socketBuyer1In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_AGREEToBuyer1(): Unit = {
        |    this.socketBuyer1Out.println("AGREE")
        |    this.socketSellerOut.println("AGREE") //also send to seller
        |
        |  }
        |
        |  def send_QUITToBuyer1(): Unit = {
        |    this.socketBuyer1Out.println("QUIT")
        |    this.socketSellerOut.println("QUIT")
        |  }
        |
        |  def send_agreeStringToBuyer1(payload: String): Unit = {
        |    this.socketBuyer1Out.println(payload)
        |  }
        |
        |  def send_agreeStringToSeller(payload: String): Unit = {
        |    this.socketSellerOut.println(payload)
        |    /* Added to avoid the "Send transfer"
        |        message appearing on both Buyer1 and Buyer2 terminals at once.*/ var ok = ""
        |    try ok = this.socketSellerIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error")
        |        System.exit(-1)
        |    }
        |  }
        |
        |  def send_transferintToSeller(payload: Int): Unit = {
        |    this.socketSellerOut.println(payload)
        |  }
        |
        |  def send_quitStringToBuyer1(payload: String): Unit = {
        |    this.socketBuyer1Out.println(payload)
        |  }
        |
        |  def send_quitStringToSeller(payload: String): Unit = {
        |    this.socketSellerOut.println(payload)
        |  }
        |}
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |
        |
        |object Buyer2Main {
        |  def safeRead(readerBuyer2: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerBuyer2.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val currentBuyer2 = new Buyer2Role
        |    // readerBuyer2 can be used to input strings, and then use them in send method invocation
        |    val readerBuyer2 = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the Buyer2 typestate
        |    val payload1 = currentBuyer2.receive_quoteintFromBuyer1
        |    System.out.println("Received book price quote from Buyer1: £" + payload1)
        |    System.out.print("Choose a label among [AGREE, QUIT]: ")
        |    safeRead(readerBuyer2) match {
        |      case "AGREE" =>
        |        currentBuyer2.send_AGREEToBuyer1()
        |        System.out.print("Send agreement message to Buyer1: ")
        |        val payload2 = safeRead(readerBuyer2)
        |        currentBuyer2.send_agreeStringToBuyer1(payload2)
        |        System.out.print("Send agreement message to Seller: ")
        |        val payload3 = safeRead(readerBuyer2)
        |        currentBuyer2.send_agreeStringToSeller(payload3)
        |        System.out.print("Send transfer to Seller: £")
        |        val payload4 = safeRead(readerBuyer2).toInt
        |        currentBuyer2.send_transferintToSeller(payload4)
        |        System.out.println("\n---Transaction complete: book sold---")
        |
        |      case "QUIT" =>
        |        currentBuyer2.send_QUITToBuyer1()
        |        System.out.print("Send quit message to Buyer1: ")
        |        val payload5 = safeRead(readerBuyer2)
        |        currentBuyer2.send_quitStringToBuyer1(payload5)
        |        System.out.print("Send quit message to Seller: ")
        |        val payload6 = safeRead(readerBuyer2)
        |        currentBuyer2.send_quitStringToSeller(payload6)
        |        System.out.println("\n---Transaction complete: no sale---")
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "seller" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
        |import java.net.{Socket, UnknownHostException}
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val AGREE, QUIT = Value
        |}
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.net.ServerSocket
        |import java.net.Socket
        |
        |//import mungo.lib.Typestate;
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\SellerProtocol.scala") class SellerRole() { // Bind the sockets
        |  var serverBuyer2: ServerSocket = null
        |  var serverBuyer1: ServerSocket = null
        |  // Connecting to the server
        |  try { // Create the sockets
        |    serverBuyer2 = new ServerSocket(20001)
        |    serverBuyer1 = new ServerSocket(20000)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Unable to listen on ports")
        |      System.exit(-1)
        |  }
        |  // Accept a client connection
        |  var socketBuyer2: Socket = null
        |  var socketBuyer1: Socket = null
        |  try {
        |    System.out.println("Accepting...")
        |    socketBuyer2 = serverBuyer2.accept
        |    System.out.println("Buyer2 accepted")
        |    System.out.println("Accepting...")
        |    socketBuyer1 = serverBuyer1.accept
        |    System.out.println("Buyer1 accepted")
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Accept failed")
        |      System.exit(-1)
        |  }
        |  // Create the read and write streams
        |  private var socketBuyer2In:BufferedReader = null
        |  private var socketBuyer2Out:PrintWriter = null
        |  private var socketBuyer1In:BufferedReader = null
        |  private var socketBuyer1Out:PrintWriter = null
        |  try {
        |    socketBuyer2In = new BufferedReader(new InputStreamReader(socketBuyer2.getInputStream))
        |    socketBuyer2Out = new PrintWriter(socketBuyer2.getOutputStream, true)
        |    socketBuyer1In = new BufferedReader(new InputStreamReader(socketBuyer1.getInputStream))
        |    socketBuyer1Out = new PrintWriter(socketBuyer1.getOutputStream, true)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Read failed")
        |      System.exit(-1)
        |  }
        |
        |
        |  def receive_bookStringFromBuyer1(): String = {
        |    var line = ""
        |    try line = this.socketBuyer1In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_bookintToBuyer1(payload: Int): Unit = {
        |    this.socketBuyer1Out.println(payload)
        |  }
        |
        |  def receive_Choice1LabelFromBuyer2(): Choice1.Value = {
        |    var stringLabelChoice1 = ""
        |    try stringLabelChoice1 = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to get label")
        |        System.exit(-1)
        |    }
        |    stringLabelChoice1 match {
        |      case "AGREE" =>
        |        System.out.println("Buyer2 Agrees")
        |        Choice1.AGREE
        |      case "QUIT" =>
        |        System.out.println("Buyer2 Refuses")
        |        Choice1.QUIT
        |      case _ =>
        |        Choice1.QUIT
        |    }
        |  }
        |
        |  def receive_agreeStringFromBuyer2(): String = {
        |    var line = ""
        |    try line = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_transferintFromBuyer1(): Int = {
        |    var line = ""
        |    try line = this.socketBuyer1In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    /* Added to avoid the "Send transfer"
        |        message appearing on both Buyer1 and Buyer2 terminals at once.*/ this.socketBuyer2Out.println("OK Buyer2")
        |    line.toInt
        |  }
        |
        |  def receive_transferintFromBuyer2(): Int = {
        |    var line = ""
        |    try line = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def receive_quitStringFromBuyer2(): String = {
        |    var line = ""
        |    try line = this.socketBuyer2In.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |}
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |
        |
        |object SellerMain {
        |  def safeRead(readerSeller: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerSeller.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val currentSeller = new SellerRole
        |    // readerSeller can be used to input strings, and then use them in send method invocation
        |    val readerSeller = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the Seller typestate
        |    val payload1 = currentSeller.receive_bookStringFromBuyer1
        |    System.out.println("Received book title from Buyer1: " + payload1)
        |    System.out.print("Send book price to Buyer1: £")
        |    val payload2 = safeRead(readerSeller).toInt
        |    currentSeller.send_bookintToBuyer1(payload2)
        |    currentSeller.receive_Choice1LabelFromBuyer2 match {
        |      case Choice1.AGREE =>
        |        val payload3 = currentSeller.receive_agreeStringFromBuyer2()
        |        System.out.println("Received agreement message from Buyer2: " + payload3)
        |        val payload4 = currentSeller.receive_transferintFromBuyer1
        |        System.out.println("Received transfer from Buyer1: £" + payload4)
        |        val payload5 = currentSeller.receive_transferintFromBuyer2
        |        System.out.println("Received transfer from Buyer2: £" + payload5)
        |        System.out.println("\n---Transaction complete: book sold---")
        |
        |      case Choice1.QUIT =>
        |        val payload6 = currentSeller.receive_quitStringFromBuyer2
        |        System.out.println("Received quit message from Buyer2: " + payload6)
        |        System.out.println("\n---Transaction complete: no sale---")
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region BuyTicket
  "A" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
        |import java.net.{Socket, UnknownHostException}
        |
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val APPROVE, REFUSE = Value
        |}
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.net.ServerSocket
        |import java.net.Socket
        |import java.net.UnknownHostException
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\AProtocol.scala") class ARole() { // Bind the sockets
        |  private var socketRIn:BufferedReader = null
        |  private var socketROut:PrintWriter = null
        |  private var socketFIn:BufferedReader = null
        |  private var socketFOut:PrintWriter = null
        |  var serverF: ServerSocket = null
        |  try // Create the sockets
        |    serverF = new ServerSocket(20002)
        |  catch {
        |    case e: IOException =>
        |      System.out.println("Unable to listen on port")
        |      System.exit(-1)
        |  }
        |  // Accept a client connection
        |  var socketF: Socket = null
        |  try {
        |    System.out.println("Accepting...")
        |    socketF = serverF.accept
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Accept failed")
        |      System.exit(-1)
        |  }
        |  // Create the Finance read and write streams
        |  try {
        |    socketFIn = new BufferedReader(new InputStreamReader(socketF.getInputStream))
        |    socketFOut = new PrintWriter(socketF.getOutputStream, true)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Read failed")
        |      System.exit(-1)
        |  }
        |  System.out.println("Accepted connection")
        |  // Connect to the servers
        |  try {
        |    val socketR = new Socket("localhost", 20001)
        |    // Create the Researcher read and write streams
        |    socketRIn = new BufferedReader(new InputStreamReader(socketR.getInputStream))
        |    socketROut = new PrintWriter(socketR.getOutputStream, true)
        |  } catch {
        |    case e: UnknownHostException =>
        |      System.out.println("Unable to connect to the remote host")
        |      System.exit(-1)
        |    case e: IOException =>
        |      System.out.println("Input/output error, unable to connect")
        |      System.exit(-1)
        |  }
        |
        |
        |  def receive_requestStringFromR(): String = {
        |    var line = ""
        |    try line = this.socketRIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_quoteintToR(payload: Int): Unit = {
        |    this.socketROut.println(payload)
        |  }
        |
        |  def receive_Choice1LabelFromF(): Choice1.Value = {
        |    var stringLabelChoice1 = ""
        |    try stringLabelChoice1 = this.socketFIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to get label")
        |        System.exit(-1)
        |    }
        |    var intLabelChoice1 = 0
        |    if (stringLabelChoice1 == "APPROVE") intLabelChoice1 = 1
        |    else if (stringLabelChoice1 == "REFUSE") intLabelChoice1 = 2
        |    intLabelChoice1 match {
        |      case 1 =>
        |        Choice1.APPROVE
        |      case 2 =>
        |        Choice1.REFUSE
        |      case _ =>
        |        Choice1.REFUSE
        |    }
        |  }
        |
        |  def receive_approveintFromF(): Int = {
        |    var line = ""
        |    try line = this.socketFIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_ticketStringToR(payload: String): Unit = {
        |    this.socketROut.println(payload)
        |  }
        |
        |  def send_invoiceintToF(payload: Int): Unit = {
        |    this.socketFOut.println(payload)
        |  }
        |
        |  def receive_paymentintFromF(): Int = {
        |    var line = ""
        |    try line = this.socketFIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def receive_refuseStringFromF(): String = {
        |    var line = ""
        |    try line = this.socketFIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |}
        |
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |
        |
        |object AMain {
        |  def safeRead(readerA: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerA.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val currentA = new ARole
        |    // readerA can be used to input strings, and then use them in send method invocation
        |    val readerA = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the A typestate
        |    val payload1 = currentA.receive_requestStringFromR
        |    System.out.println("Received travel destination request from Researcher: " + payload1)
        |    System.out.print("Send quote price to Researcher: £")
        |    val payload2 = safeRead(readerA).toInt
        |    currentA.send_quoteintToR(payload2)
        |    currentA.receive_Choice1LabelFromF match {
        |      case Choice1.APPROVE =>
        |        val payload3 = currentA.receive_approveintFromF
        |        System.out.println("Received approval code from Finance: " + payload3)
        |        System.out.print("Send ticket to Researcher: ")
        |        val payload4 = safeRead(readerA)
        |        currentA.send_ticketStringToR(payload4)
        |        System.out.print("Send invoice code to Finance: ")
        |        val payload5 = safeRead(readerA).toInt
        |        currentA.send_invoiceintToF(payload5)
        |        val payload6 = currentA.receive_paymentintFromF
        |        System.out.println("Received payment from Finance: £" + payload6)
        |        System.out.println("\n	----TRANSACTION COMPLETE----	")
        |
        |      case Choice1.REFUSE =>
        |        val payload7 = currentA.receive_refuseStringFromF
        |        System.out.println("Received refusal from Finance: " + payload7)
        |        System.out.println("\n	----TRANSACTION COMPLETE----	")
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "F" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val APPROVE, REFUSE = Value
        |}
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
        |import java.net.{Socket, UnknownHostException}
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\FProtocol.scala") class FRole() { // Connect to the other participants in the protocol
        |  private var socketRIn:BufferedReader = _
        |  private var socketROut:PrintWriter = _
        |  private var socketAIn:BufferedReader = _
        |  private var socketAOut:PrintWriter = _
        |  try { // Create the sockets
        |    val socketR = new Socket("localhost", 20000)
        |    val socketA = new Socket("localhost", 20002)
        |    socketRIn = new BufferedReader(new InputStreamReader(socketR.getInputStream))
        |    socketROut = new PrintWriter(socketR.getOutputStream, true)
        |    socketAIn = new BufferedReader(new InputStreamReader(socketA.getInputStream))
        |    socketAOut = new PrintWriter(socketA.getOutputStream, true)
        |  } catch {
        |    case e: UnknownHostException =>
        |      System.out.println("Unable to connect to the remote host")
        |      System.exit(-1)
        |    case e: IOException =>
        |      System.out.println("Input/output error")
        |      System.exit(-1)
        |  }
        |  System.out.println("Connected")
        |
        |
        |  def receive_checkintFromR(): Int = {
        |    var line = ""
        |    try line = this.socketRIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_APPROVEToR(): Unit = {
        |    this.socketROut.println("APPROVE")
        |    this.socketAOut.println("APPROVE") //also send to Agent
        |
        |  }
        |
        |  def send_REFUSEToR(): Unit = {
        |    this.socketROut.println("REFUSE")
        |    this.socketAOut.println("REFUSE")
        |  }
        |
        |  def send_approveintToR(payload: Int): Unit = {
        |    this.socketROut.println(payload)
        |  }
        |
        |  def send_approveintToA(payload: Int): Unit = {
        |    this.socketAOut.println(payload)
        |  }
        |
        |  def receive_invoiceintFromA(): Int = {
        |    var line = ""
        |    try line = this.socketAIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def send_paymentintToA(payload: Int): Unit = {
        |    this.socketAOut.println(payload)
        |  }
        |
        |  def send_refuseStringToR(payload: String): Unit = {
        |    this.socketROut.println(payload)
        |  }
        |
        |  def send_refuseStringToA(payload: String): Unit = {
        |    this.socketAOut.println(payload)
        |  }
        |}
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader}
        |
        |
        |object FMain {
        |  def safeRead(readerF: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerF.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val currentF = new FRole
        |    // readerF can be used to input strings, and then use them in send method invocation
        |    val readerF = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the F typestate
        |    val payload1 = currentF.receive_checkintFromR()
        |    System.out.println("Received quote price from Researcher: £" + payload1)
        |    System.out.print("Choose a label among [APPROVE, REFUSE]: ")
        |    safeRead(readerF) match {
        |      case "APPROVE" =>
        |        currentF.send_APPROVEToR()
        |        System.out.print("Send approval code to Researcher: ")
        |        val payload2 = safeRead(readerF).toInt
        |        currentF.send_approveintToR(payload2)
        |        System.out.print("Send approval code to Agent: ")
        |        val payload3 = safeRead(readerF).toInt
        |        currentF.send_approveintToA(payload3)
        |        val payload4 = currentF.receive_invoiceintFromA()
        |        System.out.println("Received invoice from Agent: " + payload4)
        |        System.out.print("Send payment to Agent: £")
        |        val payload5 = safeRead(readerF).toInt
        |        currentF.send_paymentintToA(payload5)
        |        System.out.println("\n	----TRANSACTION COMPLETE----	")
        |
        |      case "REFUSE" =>
        |        currentF.send_REFUSEToR()
        |        System.out.print("Send travel refusal to Researcher: ")
        |        val payload6 = safeRead(readerF)
        |        currentF.send_refuseStringToR(payload6)
        |        System.out.print("Send travel refusal to Agent: ")
        |        val payload7 = safeRead(readerF)
        |        currentF.send_refuseStringToA(payload7)
        |        System.out.println("\n	----TRANSACTION COMPLETE----	")
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "R" should "not throw an exception" in {
    val userCode =
      """
        |import java.io.BufferedReader;
        |import java.io.IOException;
        |import java.io.InputStreamReader;
        |
        |public class RMain {
        |    public static String safeRead(BufferedReader readerR) {
        |        String readline = "";
        |        try {
        |            readline = readerR.readLine();
        |        } catch (IOException e) {
        |            System.out.println("Input/Output error, unable to read");
        |            System.exit(-1);
        |        }
        |        return readline;
        |    }
        |
        |    public static void main(String[] args) {
        |        // Create the current role
        |        RRole currentR = new RRole();
        |        // readerR can be used to input strings, and then use them in send method invocation
        |        BufferedReader readerR = new BufferedReader(new InputStreamReader(System.in));
        |        // Method invocation follows the R typestate
        |        System.out.print("Send travel destination request to Agent: ");
        |        String payload1 = safeRead(readerR);
        |        currentR.send_requestStringToA(payload1);
        |        int payload2 = currentR.receive_quoteintFromA();
        |        System.out.println("Received quote price from Agent: £" + payload2);
        |        System.out.print("Send quote price to Finance: £");
        |        int payload3 = Integer.parseInt(safeRead(readerR));
        |        currentR.send_checkintToF(payload3);
        |        switch (currentR.receive_Choice1LabelFromF()) {
        |            case APPROVE:
        |                System.out.println("Finance has approved the request");
        |                int payload4 = currentR.receive_approveintFromF();
        |                System.out.println("Received approve code from Finance: " + payload4);
        |                String payload5 = currentR.receive_ticketStringFromA();
        |                System.out.println("Received ticket from Agent: " + payload5);
        |                System.out.println("\n	----TRANSACTION COMPLETE----	");
        |                break;
        |            case REFUSE:
        |                System.out.println("Finance has refused the request");
        |                String payload6 = currentR.receive_refuseStringFromF();
        |                System.out.println("Received refusal from Finance: " + payload6);
        |                System.out.println("\n	----TRANSACTION COMPLETE----	");
        |                break;
        |        }
        |    }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region pop3
  "pop3" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val OK, ERR = Value
        |}
        |
        |object Choice2 extends Enumeration {
        |  type Choice2 = Value
        |  val DOT, SUM = Value
        |}
        |
        |
        |import javax.net.ssl.SSLSocket
        |import javax.net.ssl.SSLSocketFactory
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.PrintWriter
        |
        |import compilerPlugin.CRole.{sslIn, sslOut}
        |import compilerPlugin.Choice1.OK
        |import compilerPlugin.Choice2.DOT
        |
        |import scala.util.control.Breaks
        |
        |object IntString {
        |  val sep = " "
        |
        |  //static string needed for static parser
        |  // take substring
        |  // +OK Int String
        |  // OK Int String
        |  def Parse(message: String): IntString = {
        |    val substring = message.substring(4, message.length)
        |    val pieces = substring.split(sep)
        |    new IntString(pieces(0).toInt, pieces(1))
        |  }
        |}
        |
        |class IntString(var x: Int, var y: String) {
        |  override def toString: String = {
        |    "OK " + this.x + IntString.sep + this.y
        |  }
        |}
        |
        |object SUMIntString {
        |  val sep = " "
        |
        |  //static string needed for static parser
        |  // take substring
        |  // +OK Int String
        |  // OK Int String
        |  def Parse(message: String): SUMIntString = {
        |    val pieces = message.split(sep)
        |    new SUMIntString(pieces(0).toInt, pieces(1))
        |  }
        |}
        |
        |class SUMIntString(var x: Int, var y: String) {
        |  override def toString: String = {
        |    this.x + SUMIntString.sep + this.y
        |  }
        |}
        |
        |
        |object SUMString {
        |  def Parse(message: String) = new SUMString(message)
        |}
        |
        |class SUMString(var a: String) {
        |  override def toString: String = {
        |    this.a
        |  }
        |}
        |
        |object SUMTwoInt {
        |  val sep = " "
        |
        |  // take substring
        |  // use PairInt.java
        |  // +OK Int Int
        |  // OK Int Int
        |  def Parse(message: String): SUMTwoInt = {
        |    val r = PairInt.Parse(message)
        |    new SUMTwoInt(r.x, r.y)
        |  }
        |}
        |
        |class SUMTwoInt(var x: Int, var y: Int) {
        |  override def toString: String = {
        |    this.x + SUMTwoInt.sep + this.y
        |  }
        |}
        |
        |object PairInt {
        |  val sep = " "
        |  def Parse(message: String): PairInt = { //input: "Int Int"
        |    val pieces = message.split(sep)
        |    new PairInt(pieces(0).toInt, pieces(1).toInt)
        |  }
        |}
        |
        |class PairInt(var x: Int, var y: Int) {
        |  override def toString: String = {
        |    //output: "Int Int"
        |    this.x + PairInt.sep + this.y
        |  }
        |}
        |
        |object TwoInt {
        |  val sep = " "
        |  // take substring
        |  // use PairInt.java
        |  // +OK Int Int
        |  // OK Int Int
        |  def Parse(message: String): TwoInt = {
        |    val substring = message.substring(4, message.length)
        |    val r = PairInt.Parse(substring)
        |    new TwoInt(r.x, r.y)
        |  }
        |}
        |
        |class TwoInt(var x: Int, var y: Int) {
        |  override def toString: String = {
        |    this.x + TwoInt.sep + this.y
        |  }
        |}
        |
        |object ERRString { //static String sep = "-";
        |  def Parse(message: String): ERRString = {
        |    var substring = ""
        |    if (message != null) substring = message.substring(4, message.length)
        |    new ERRString(substring)
        |  }
        |}
        |
        |class ERRString(var a: String) {
        |  override def toString: String = {
        |    val message = "ERR " + this.a
        |    message
        |  }
        |}
        |
        |object OKString {
        |  def Parse(message: String): OKString = {
        |    var substring = ""
        |    if (message != null) substring = message.substring(3, message.length)
        |    new OKString(substring)
        |  }
        |}
        |
        |class OKString(var a: String) {
        |  val label = "OK "
        |
        |  override def toString: String = {
        |    val message = this.label + this.a
        |    message
        |  }
        |}
        |
        |object CRole{
        |  val sslSocketFactory: SSLSocketFactory = SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
        |  var sslSocket: SSLSocket = null
        |  var sslIn: BufferedReader = null
        |  var sslOut: PrintWriter = null
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\Pop3CProtocol.scala")
        |class CRole {
        |  var currentmessage: String = null //to store server messages in
        |  //reading server responses
        |  def Servermessage: String = {
        |    try this.currentmessage = sslIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error.")
        |        System.exit(-1)
        |    }
        |    this.currentmessage
        |  }
        |
        |  def receive_OKNStringFromS(): OKString = {
        |    this.Servermessage
        |    val okn = OKString.Parse(this.currentmessage)
        |    okn
        |  }
        |
        |  def send_USERToS(): Unit = {
        |    //this.socketSOut.println("USER");
        |  }
        |
        |  def send_QUITToS(): Unit = {
        |    //this.socketSOut.println("QUIT");
        |  }
        |
        |  def send_USERStringToS(payload: String): Unit = {
        |    sslOut.println("USER " + payload)
        |  }
        |
        |  def receive_Choice1LabelFromS(): Choice1.Value = {
        |    this.Servermessage
        |    if (currentmessage != null && this.currentmessage.charAt(0) == '+') Choice1.OK
        |    else Choice1.ERR
        |  }
        |
        |  def receive_OKStringFromS(): OKString = {
        |    val ok = OKString.Parse(this.currentmessage)
        |    ok
        |  }
        |
        |  def send_PASSToS(): Unit = {
        |    //this.socketSOut.println("PASS");
        |  }
        |
        |  def send_PASSStringToS(payload: String): Unit = {
        |    sslOut.println("PASS " + payload)
        |  }
        |
        |  def send_STATToS(): Unit = {
        |    //this.socketSOut.println("STAT");
        |  }
        |
        |  def send_LISTToS(): Unit = {
        |    //this.socketSOut.println("LIST");
        |  }
        |
        |  def send_LIST_NToS(): Unit = {
        |    //this.socketSOut.println("LIST_N");
        |  }
        |
        |  def send_RETR_NToS(): Unit = {
        |    //this.socketSOut.println("RETR_N");
        |  }
        |
        |  def send_DELE_NToS(): Unit = {
        |    //this.socketSOut.println("DELE_N");
        |  }
        |
        |  def send_RSETToS(): Unit = {
        |    //this.socketSOut.println("RSET");
        |  }
        |
        |  def send_TOP_NToS(): Unit = {
        |    //this.socketSOut.println("TOP_N");
        |  }
        |
        |  def send_NOOPToS(): Unit = {
        |    //this.socketSOut.println("NOOP");
        |  }
        |
        |  def send_UIDLToS(): Unit = {
        |    //this.socketSOut.println("UIDL");
        |  }
        |
        |  def send_UIDL_NToS(): Unit = {
        |    //this.socketSOut.println("UIDL_N");
        |  }
        |
        |  def send_STATVoidToS(payload: Null): Unit = {
        |    sslOut.println("STAT")
        |  }
        |
        |  def receive_OKNTwoIntFromS(): TwoInt = {
        |    this.Servermessage
        |    TwoInt.Parse(this.currentmessage)
        |  }
        |
        |  def send_LISTVoidToS(payload: Null): Unit = {
        |    sslOut.println("LIST")
        |  }
        |
        |  def receive_Choice2LabelFromS(): Choice2.Value = {
        |    this.Servermessage
        |    if (this.currentmessage.charAt(0) == '.') Choice2.DOT
        |    else Choice2.SUM
        |  }
        |
        |  def receive_DOTVoidFromS(): Null = null
        |
        |  def receive_SUMTwoIntFromS(): SUMTwoInt = { //sum is always part of choice
        |    SUMTwoInt.Parse(this.currentmessage)
        |  }
        |
        |  def receive_ERRStringFromS(): ERRString = { //always part of choice
        |    ERRString.Parse(this.currentmessage)
        |  }
        |
        |  def send_LIST_nIntToS(payload: Int): Unit = {
        |    sslOut.println("LIST " + payload)
        |  }
        |
        |  def receive_OKTwoIntFromS(): TwoInt = { //part of choice - do not update servermessage
        |    TwoInt.Parse(this.currentmessage)
        |  }
        |
        |  def send_RETR_nIntToS(payload: Int): Unit = {
        |    sslOut.println("RETR " + payload)
        |  }
        |
        |  def receive_SUMStringFromS(): SUMString = { //this.Servermessage(); - part of choice
        |    SUMString.Parse(this.currentmessage)
        |  }
        |
        |  def send_DELE_nIntToS(payload: Int): Unit = {
        |    sslOut.println("DELE " + payload)
        |  }
        |
        |  def send_RSETVoidToS(payload: Null): Unit = {
        |    sslOut.println("RSET")
        |  }
        |
        |  def send_TOP_nTwoIntToS(payload: TwoInt): Unit = {
        |    sslOut.println("TOP " + payload)
        |  }
        |
        |  def send_NOOPVoidToS(payload: Null): Unit = {
        |    sslOut.println("NOOP")
        |  }
        |
        |  def receive_OKNVoidFromS(): Null = null
        |
        |  def send_QUITVoidToS(payload: Null): Unit = {
        |    sslOut.println("QUIT")
        |  }
        |
        |  def send_UIDLVoidToS(payload: Null): Unit = {
        |    sslOut.println("UIDL")
        |  }
        |
        |  def receive_SUMIntStringFromS(): SUMIntString = { //part of choice - do not call new servermessage
        |    SUMIntString.Parse(this.currentmessage)
        |  }
        |
        |  def send_UIDL_nIntToS(payload: Int): Unit = {
        |    sslOut.println("UIDL " + payload)
        |  }
        |
        |  def receive_OKIntStringFromS(): IntString = {
        |    IntString.Parse(this.currentmessage)
        |  }
        |}
        |
        |
        |
        |import javax.net.ssl.SSLSocket
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.util.Scanner
        |
        |
        |object CMain {
        |  val CRLF = "\\r\\n"
        |
        |  def safeRead(readerC: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerC.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = {
        |    try {
        |      System.out.println("Starting SSL/POP3 test...\n")
        |      System.out.println("Creating socket connection.")
        |      CRole.sslSocket = CRole.sslSocketFactory.createSocket("pop.gmx.co.uk", 995).asInstanceOf[SSLSocket]
        |      CRole.sslIn = new BufferedReader(new InputStreamReader(CRole.sslSocket.getInputStream))
        |      CRole.sslOut = new PrintWriter(CRole.sslSocket.getOutputStream, true)
        |      // Create the current role
        |      val currentC = new CRole
        |      // readerC can be used to input strings, and then use them in send method invocation
        |      val readerC = new BufferedReader(new InputStreamReader(System.in))
        |      // Method invocation follows the C typestate
        |      val payload1 = currentC.receive_OKNStringFromS()
        |      System.out.println("Received from S: " + payload1)
        |      val usernameAuth = new Breaks
        |      val usernameAuthInner = new Breaks
        |      val passwordAuth = new Breaks
        |      val passwordAuthInner = new Breaks
        |      val transaction = new Breaks
        |      val transactionInner = new Breaks
        |      val summaryChoiceUidl = new Breaks
        |      val summaryChoiceTop = new Breaks
        |      val summaryChoiceRetrieve = new Breaks
        |      val summaryChoiceList = new Breaks
        |      usernameAuth.breakable {
        |        do {
        |          usernameAuthInner.breakable{
        |          System.out.print("Choose a label among USER or QUIT: ")
        |          safeRead(readerC) match {
        |            case "USER" =>
        |              currentC.send_USERToS()
        |              System.out.print("Send username to S: ")
        |              val payload2 = safeRead(readerC)
        |              currentC.send_USERStringToS(payload2)
        |              currentC.receive_Choice1LabelFromS() match {
        |                case OK =>
        |                  val payload3 = currentC.receive_OKStringFromS()
        |                  System.out.println("Received from S: " + payload3)
        |                  passwordAuth.breakable {
        |                    do {
        |                      passwordAuthInner.breakable {
        |                        System.out.print("Choose a label among PASS or QUIT: ")
        |                        safeRead(readerC) match {
        |                          case "PASS" =>
        |                            currentC.send_PASSToS()
        |                            System.out.print("Send password to S: ")
        |                            val payload4 = safeRead(readerC)
        |                            currentC.send_PASSStringToS(payload4)
        |                            currentC.receive_Choice1LabelFromS() match {
        |                              case OK =>
        |                                val payload5 = currentC.receive_OKStringFromS()
        |                                System.out.println("Received from S: " + payload5)
        |                                transaction.breakable {
        |                                  do {
        |                                    transactionInner.breakable {
        |                                      System.out.print("Choose a label among [STAT, LIST, LIST_N, RETR_N, DELE_N, RSET, TOP_N, NOOP, QUIT, UIDL, UIDL_N]: ")
        |                                      safeRead(readerC) match {
        |                                        case "STAT" =>
        |                                          currentC.send_STATToS()
        |                                          val payload6 = null
        |                                          currentC.send_STATVoidToS(payload6)
        |                                          val payload7 = currentC.receive_OKNTwoIntFromS()
        |                                          System.out.println("Received from S: OK " + payload7)
        |                                          transactionInner.break()
        |                                        case "LIST" =>
        |                                          currentC.send_LISTToS()
        |                                          val payload8 = null
        |                                          currentC.send_LISTVoidToS(payload8)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload9 = currentC.receive_OKStringFromS()
        |                                              System.out.println("Received from S: " + payload9)
        |                                              do {
        |                                                summaryChoiceList.breakable{
        |                                                currentC.receive_Choice2LabelFromS() match {
        |                                                  case DOT =>
        |                                                    val payload10 = currentC.receive_DOTVoidFromS()
        |                                                    System.out.println("Received from S: .")
        |                                                    transactionInner.break()
        |                                                  case Choice2.SUM =>
        |                                                    val payload11 = currentC.receive_SUMTwoIntFromS()
        |                                                    System.out.println("Received from S: " + payload11)
        |                                                    summaryChoiceList.break()
        |                                                  }
        |                                                }
        |                                              } while (true)
        |                                            case Choice1.ERR =>
        |                                              val payload12 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload12)
        |                                              transactionInner.break()
        |                                          }
        |                                          transactionInner.break()
        |                                        case "LIST_N" =>
        |                                          currentC.send_LIST_NToS()
        |                                          System.out.print("Send messagenumber to S: ")
        |                                          val keyboard1 = new Scanner(System.in) //read keyboard
        |                                          val payload13 = keyboard1.nextInt //to declare payload13
        |                                          currentC.send_LIST_nIntToS(payload13)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload14 = currentC.receive_OKTwoIntFromS()
        |                                              System.out.println("Received from S: OK " + payload14)
        |                                              transactionInner.break()
        |
        |                                            case Choice1.ERR =>
        |                                              val payload15 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload15)
        |                                              transactionInner.break()
        |
        |                                          }
        |                                          transactionInner.break()
        |                                        case "RETR_N" =>
        |                                          currentC.send_RETR_NToS()
        |                                          System.out.print("Send messagenumber to S: ")
        |                                          val keyboard2 = new Scanner(System.in)
        |                                          val payload16 = keyboard2.nextInt //to declare payload16
        |                                          currentC.send_RETR_nIntToS(payload16)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload17 = currentC.receive_OKStringFromS()
        |                                              System.out.println("Received from S: " + payload17)
        |                                              do
        |                                                summaryChoiceRetrieve.breakable {
        |                                                  currentC.receive_Choice2LabelFromS() match {
        |                                                    case DOT =>
        |                                                      val payload18 = currentC.receive_DOTVoidFromS()
        |                                                      //System.out.println("Received from S: " + payload18);
        |                                                      System.out.println("Received from S: .")
        |                                                      //break _summary_choice_retrieve;
        |                                                      transactionInner.break()
        |                                                    case Choice2.SUM =>
        |                                                      val payload19 = currentC.receive_SUMStringFromS()
        |                                                      //System.out.println("Received from S: " + payload19);
        |                                                      System.out.println(payload19)
        |                                                      summaryChoiceRetrieve.break()
        |                                                  }
        |                                                } while (true)
        |                                            case Choice1.ERR =>
        |                                              val payload20 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload20)
        |                                              transactionInner.break()
        |
        |                                          }
        |                                          transactionInner.break()
        |
        |                                        case "DELE_N" =>
        |                                          currentC.send_DELE_NToS()
        |                                          System.out.print("Send messagenumber to S: ")
        |                                          val keyboard3 = new Scanner(System.in)
        |                                          val payload21 = keyboard3.nextInt //to declare payload21
        |                                          currentC.send_DELE_nIntToS(payload21)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload22 = currentC.receive_OKStringFromS()
        |                                              System.out.println("Received from S: " + payload22)
        |                                              transactionInner.break()
        |
        |                                            case Choice1.ERR =>
        |                                              val payload23 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload23)
        |                                              transactionInner.break()
        |
        |                                          }
        |                                          transactionInner.break()
        |
        |                                        case "RSET" =>
        |                                          currentC.send_RSETToS()
        |                                          val payload24 = null
        |                                          currentC.send_RSETVoidToS(payload24)
        |                                          val payload25 = currentC.receive_OKNStringFromS()
        |                                          System.out.println("Received from S: " + payload25)
        |                                          transactionInner.break()
        |
        |                                        case "TOP_N" =>
        |                                          currentC.send_TOP_NToS()
        |                                          //System.out.print("Send messagenumber and number of lines to S: ");
        |                                          val keyboard6 = new Scanner(System.in)
        |                                          System.out.print("Send messagenumber to S: ")
        |                                          val number1 = keyboard6.nextInt
        |                                          System.out.print("Send number of lines to S: ")
        |                                          val number2 = keyboard6.nextInt
        |                                          val payload26 = new TwoInt(number1, number2)
        |                                          //String payload26 = safeRead(readerC);
        |                                          currentC.send_TOP_nTwoIntToS(payload26)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload27 = currentC.receive_OKStringFromS()
        |                                              System.out.println("Received from S: " + payload27)
        |                                              do
        |                                                summaryChoiceTop.breakable {
        |                                                  currentC.receive_Choice2LabelFromS() match {
        |                                                    case DOT =>
        |                                                      val payload28 = currentC.receive_DOTVoidFromS()
        |                                                      System.out.println("Received from S: .")
        |                                                      //System.out.println("Received from S: " + payload28);
        |                                                      //break _summary_choice_top;
        |                                                      transactionInner.break()
        |                                                    case Choice2.SUM =>
        |                                                      val payload29 = currentC.receive_SUMStringFromS()
        |                                                      System.out.println(/*"Received from S: " + */ payload29)
        |                                                      summaryChoiceTop.break()
        |                                                  }
        |                                                }while ( {
        |                                                true
        |                                              })
        |                                            case Choice1.ERR =>
        |                                              val payload30 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload30)
        |                                              transactionInner.break()
        |
        |                                          }
        |                                          transactionInner.break()
        |
        |                                        case "NOOP" =>
        |                                          currentC.send_NOOPToS()
        |                                          val payload31 = null
        |                                          currentC.send_NOOPVoidToS(payload31)
        |                                          val payload32 = currentC.receive_OKNVoidFromS()
        |                                          System.out.println("Received from S: " + payload32)
        |
        |                                          transactionInner.break()
        |
        |                                        case "QUIT" =>
        |                                          currentC.send_QUITToS()
        |                                          val payload33 = null
        |                                          currentC.send_QUITVoidToS(payload33)
        |                                          val payload34 = currentC.receive_OKNStringFromS()
        |                                          System.out.println("Received from S: " + payload34)
        |                                          transaction.break()
        |
        |                                        case "UIDL" =>
        |                                          currentC.send_UIDLToS()
        |                                          val payload35 = null
        |                                          currentC.send_UIDLVoidToS(payload35)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload36 = currentC.receive_OKStringFromS()
        |                                              System.out.println("Received from S: " + payload36)
        |                                              do
        |                                                summaryChoiceUidl.breakable {
        |                                                currentC.receive_Choice2LabelFromS() match {
        |                                                  case DOT =>
        |                                                  val payload37 = currentC.receive_DOTVoidFromS()
        |                                                  System.out.println ("Received from S: .")
        |                                                    //System.out.println("Received from S: " + payload37);
        |                                                    //break _summary_choice_uidl;
        |                                                  transactionInner.break ()
        |                                                  case Choice2.SUM =>
        |                                                  val payload38 = currentC.receive_SUMIntStringFromS()
        |                                                  System.out.println ("Received from S: " + payload38)
        |                                                  summaryChoiceUidl.break()
        |                                                }
        |                                              }while ( {
        |                                                true
        |                                              })
        |                                            case Choice1.ERR =>
        |                                              val payload39 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload39)
        |                                              transactionInner.break()
        |                                          }
        |                                          transactionInner.break()
        |                                        case "UIDL_N" =>
        |                                          currentC.send_UIDL_NToS()
        |                                          System.out.print("Send messagenumber to S: ")
        |                                          val keyboard4 = new Scanner(System.in)
        |                                          val payload40 = keyboard4.nextInt //to declare payload40
        |                                          currentC.send_UIDL_nIntToS(payload40)
        |                                          currentC.receive_Choice1LabelFromS() match {
        |                                            case OK =>
        |                                              val payload41 = currentC.receive_OKIntStringFromS()
        |                                              System.out.println("Received from S: " + payload41)
        |                                              transactionInner.break()
        |                                            case Choice1.ERR =>
        |                                              val payload42 = currentC.receive_ERRStringFromS()
        |                                              System.out.println("Received from S: " + payload42)
        |                                              transactionInner.break()
        |                                            }
        |                                          transactionInner.break()
        |                                      }
        |                                    }
        |                                  } while (true)
        |                                }
        |                                passwordAuth.break()
        |                              case Choice1.ERR =>
        |                                val payload43 = currentC.receive_ERRStringFromS()
        |                                System.out.println("Received from S: " + payload43)
        |                                passwordAuthInner.break()
        |                            }
        |                            passwordAuth.break()
        |
        |                          case "QUIT" =>
        |                            currentC.send_QUITToS()
        |                            val payload44 = null
        |                            currentC.send_QUITVoidToS(payload44)
        |                            val payload45 = currentC.receive_OKNStringFromS()
        |                            System.out.println("Received from S: " + payload45)
        |                            passwordAuth.break()
        |                        }
        |                      }
        |                    } while ( {
        |                      true
        |                    })
        |                  }
        |                  usernameAuth.break()
        |
        |                case Choice1.ERR =>
        |                  val payload46 = currentC.receive_ERRStringFromS()
        |                  System.out.println("Received from S: " + payload46)
        |                  usernameAuthInner.break()
        |              }
        |              usernameAuth.break()
        |            case "QUIT" =>
        |              currentC.send_QUITToS()
        |              val payload47 = null
        |              currentC.send_QUITVoidToS(payload47)
        |              val payload48 = currentC.receive_OKNStringFromS()
        |              System.out.println("Received from S: " + payload48)
        |              usernameAuth.break()
        |          }
        |        } }while ( {
        |          true
        |        })
        |      }
        |      CRole.sslIn.close
        |      CRole.sslOut.close
        |    } catch {
        |      case e: IOException =>
        |        System.out.println("Input/output error")
        |        System.exit(-1)
        |    } //end of try
        |
        |    //end of main
        |  }
        |    //end of class
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region SMTP
  "SMTPClient" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |import javax.net.ssl.SSLSocket
        |import java.io.BufferedReader
        |import java.io.IOException
        |import java.io.InputStreamReader
        |import java.io.PrintWriter
        |import java.net.Socket
        |import java.net.UnknownHostException
        |
        |import scala.util.control.Breaks
        |
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val _250DASH, _250 = Value
        |}
        |
        |object Choice2 extends Enumeration {
        |  type Choice2 = Value
        |  val _235, _535 = Value
        |}
        |
        |object Choice3 extends Enumeration {
        |  type Choice3 = Value
        |  val _501, _250 = Value
        |}
        |
        |object Choice4 extends Enumeration {
        |  type Choice4 = Value
        |  val _250 = Value
        |}
        |
        |object SMTPMessage {
        |  def Parse(message: String): SMTPMessage = {
        |    val matches = message.split(" |-", 2)
        |    new SMTPMessage(matches(0), matches(1), message.charAt(3) == '-')
        |  }
        |}
        |
        |class SMTPMessage {
        |  private var command:String = null
        |  private var payload:String = null
        |  private var isDashed = false
        |
        |  def this(command: String, payload: String, isDashed: Boolean) {
        |    this()
        |    this.command = command
        |    this.payload = payload
        |    this.isDashed = isDashed
        |  }
        |
        |  def this(command: String, payload: String) {
        |    this()
        |    this.command = command
        |    this.payload = payload
        |    this.isDashed = false
        |  }
        |
        |  def this(command: String) {
        |    this()
        |    this.command = command
        |    this.payload = null
        |    this.isDashed = false
        |  }
        |
        |  override def toString: String = {
        |    var message:String = null
        |    if (this.payload == null) message = this.command + "\\r\\n"
        |    else message = this.command + " " + this.payload + "\\r\\n"
        |    message
        |  }
        |
        |  def getCommand: String = this.command
        |
        |  def getPayload: String = this.payload
        |
        |  def getIsDashed: Boolean = this.isDashed
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\SMTPCProtocol.scala") class CRole {
        |  var socketSIn: BufferedReader = null
        |  var socketSOut: PrintWriter = null
        |  var socketS: Socket = null
        |  var sslSocket: SSLSocket = null
        |  try { //socketS = new Socket("smtp.gmail.com", 25);
        |    socketS = new Socket("smtp.gmail.com", 587)
        |    socketSIn = new BufferedReader(new InputStreamReader(socketS.getInputStream))
        |    socketSOut = new PrintWriter(socketS.getOutputStream, true)
        |  } catch {
        |    case e: UnknownHostException =>
        |      System.out.println("Unable to connect to the remote host")
        |      System.exit(-(1))
        |    case e: IOException =>
        |      System.out.println("Input/output error")
        |      System.exit(-(1))
        |  }
        |
        |  // Typestate method definitions
        |  def receive_220StringFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error.")
        |        System.exit(-(1))
        |    }
        |    return line
        |  }
        |
        |  def send_EHLOToS(): Unit = {
        |    // This method corresponds to selecting the command EHLO,
        |    // hence its body is empty.
        |  }
        |
        |  def send_QUITToS(): Unit = {
        |    // This method corresponds to selecting the command QUIT,
        |  }
        |
        |  def send_ehloStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def receive_CChoice1LabelFromS(): Choice1.Value = {
        |    var stringLabelChoice1 = ""
        |    try {
        |      stringLabelChoice1 = this.socketSIn.readLine
        |      System.out.println(stringLabelChoice1)
        |    } catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label")
        |        System.exit(-(1))
        |    }
        |    val message = SMTPMessage.Parse(stringLabelChoice1)
        |    val intLabelChoice1 = message.getCommand.toInt
        |    intLabelChoice1 match {
        |      case 250 =>
        |        if (message.getIsDashed) {
        |          Choice1._250DASH
        |        }
        |        else {
        |          Choice1._250
        |        }
        |      case _ =>
        |        Choice1._250
        |    }
        |  }
        |
        |  def receive_250dashStringFromS(): String = {
        |    return ""
        |  }
        |
        |  def receive_250StringFromS(): String = {
        |    return ""
        |  }
        |
        |  def send_STARTTLSToS(): Unit = {
        |    // This method corresponds to selecting the command STARTTLS,
        |  }
        |
        |  def send_starttlsStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def send_AUTHToS(): Unit = {
        |    // This method corresponds to selecting the command AUTH,
        |  }
        |
        |  def send_authStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def receive_CChoice2LabelFromS(): Choice2.Value = {
        |    var stringLabelChoice2 = ""
        |    try stringLabelChoice2 = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label")
        |        System.exit(-(1))
        |    }
        |    val message = SMTPMessage.Parse(stringLabelChoice2)
        |    val intLabelChoice2 = message.getCommand.toInt
        |    intLabelChoice2 match {
        |      case 235 =>
        |        Choice2._235
        |      case 535 =>
        |        Choice2._535
        |      case _ =>
        |        Choice2._535
        |    }
        |  }
        |
        |  def receive_235StringFromS(): String = {
        |    return ""
        |  }
        |
        |  def send_MAILToS(): Unit = {
        |    // This method corresponds to selecting the command MAIL,
        |  }
        |
        |  def send_mailStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def receive_CChoice3LabelFromS(): Choice3.Value = {
        |    var stringLabelChoice3 = ""
        |    try stringLabelChoice3 = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label")
        |        System.exit(-(1))
        |    }
        |    val message = SMTPMessage.Parse(stringLabelChoice3)
        |    val intLabelChoice3 = message.getCommand.toInt
        |    intLabelChoice3 match {
        |      case 501 =>
        |        Choice3._501
        |      case 250 =>
        |        Choice3._250
        |      case _ =>
        |        Choice3._250
        |    }
        |  }
        |
        |  def receive_501StringFromS(): String = {
        |    return ""
        |  }
        |
        |  def send_RCPTToS(): Unit = {
        |    // This method corresponds to selecting the command RCPT,
        |  }
        |
        |  def send_DATAToS(): Unit = {
        |    // This method corresponds to selecting the command DATA,
        |  }
        |
        |  def send_rcptStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def receive_CChoice4LabelFromS(): Choice4.Value = {
        |    var stringLabelChoice4 = ""
        |    try stringLabelChoice4 = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label")
        |        System.exit(-(1))
        |    }
        |    val message = SMTPMessage.Parse(stringLabelChoice4)
        |    val intLabelChoice4 = message.getCommand.toInt
        |    intLabelChoice4 match {
        |      case 250 =>
        |        Choice4._250
        |      case _ =>
        |        Choice4._250
        |    }
        |  }
        |
        |  def send_dataStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def receive_354StringFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error.")
        |        System.exit(-(1))
        |    }
        |    return line
        |  }
        |
        |  def send_DATALINEToS(): Unit = {
        |    // This method corresponds to selecting the command DATALINE,
        |  }
        |
        |  def send_SUBJECTToS(): Unit = {
        |    // This method corresponds to selecting the command SUBJECT,
        |  }
        |
        |  def send_ATADToS(): Unit = {
        |    // This method corresponds to selecting the command ATAD,
        |  }
        |
        |  def send_datalineStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def send_subjectStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def send_atadStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def send_quitStringToS(payload: String): Unit = {
        |    this.socketSOut.print(payload)
        |    this.socketSOut.flush()
        |  }
        |
        |  def receive_535StringFromS(): String = {
        |    return ""
        |  }
        |}
        |
        |import compilerPlugin.CRole
        |import sun.misc.BASE64Encoder
        |import javax.net.ssl.SSLSocket
        |import javax.net.ssl.SSLSocketFactory
        |import java.io._
        |import java.net.UnknownHostException
        |
        |
        |object CMain {
        |  val CRLF: String = "\\r\\n"
        |
        |  def safeRead(readerC: BufferedReader): String = {
        |    var readline: String = ""
        |    try readline = readerC.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-(1))
        |    }
        |    return readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val X = new Breaks
        |    val XInner = new Breaks
        |    val X1 = new Breaks
        |    val X1Inner = new Breaks
        |    val Y = new Breaks
        |    val YInner = new Breaks
        |    val Z1 = new Breaks
        |    val Z1Inner = new Breaks
        |    val Z2 = new Breaks
        |    val Z2Inner = new Breaks
        |    val Z3 = new Breaks
        |    val Z3Inner = new Breaks
        |    val currentC = new CRole
        |    // readerC can be used to input strings, and then use them in send method invocation
        |    val readerC: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the C typestate
        |    val payload1: SMTPMessage = SMTPMessage.Parse(currentC.receive_220StringFromS)
        |    System.out.println("Received from S: " + payload1)
        |    System.out.print("Choose a label among EHLO or QUIT: ")
        |    safeRead(readerC) match {
        |      case "EHLO" =>
        |        currentC.send_EHLOToS()
        |        System.out.print("Send to S text for EHLO: ")
        |        val payload2: String = safeRead(readerC)
        |        currentC.send_ehloStringToS((new SMTPMessage("EHLO", payload2)).toString)
        |        X.breakable {
        |          do {
        |            XInner.breakable {
        |              {
        |                currentC.receive_CChoice1LabelFromS() match {
        |                  case Choice1._250DASH =>
        |                    val payload3: String = currentC.receive_250dashStringFromS()
        |                    XInner.break()
        |                  case Choice1._250 =>
        |                    val payload4: String = currentC.receive_250StringFromS()
        |                    System.out.print("Choose a label among STARTTLS or QUIT: ")
        |                    safeRead(readerC) match {
        |                      case "STARTTLS" =>
        |                        currentC.send_STARTTLSToS()
        |                        currentC.send_starttlsStringToS((new SMTPMessage("STARTTLS")).toString)
        |                        val payload6: SMTPMessage = SMTPMessage.Parse(currentC.receive_220StringFromS)
        |                        try {
        |                          currentC.sslSocket = (SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]).createSocket(currentC.socketS, currentC.socketS.getInetAddress.getHostAddress, currentC.socketS.getPort, true).asInstanceOf[SSLSocket]
        |                          currentC.socketSIn = new BufferedReader(new InputStreamReader(currentC.sslSocket.getInputStream))
        |                          currentC.socketSOut = new PrintWriter(currentC.sslSocket.getOutputStream, true)
        |                        } catch {
        |                          case e: UnknownHostException =>
        |                            System.out.println("Unable to connect to the remote host")
        |                            System.exit(-(1))
        |                          case e: IOException =>
        |                            System.out.println("Input/output error")
        |                            System.exit(-(1))
        |                        }
        |                        System.out.println("Received from S: " + payload6)
        |                        System.out.print("Choose a label among EHLO, or QUIT: ")
        |                        val label3: Int = if (safeRead(readerC) == "EHLO") {
        |                          1
        |                        }
        |                        else {
        |                          2
        |                        }
        |                        label3 match {
        |                          case 1 =>
        |                            currentC.send_EHLOToS()
        |                            System.out.print("Send to S text for EHLO: ")
        |                            val payload7: String = safeRead(readerC)
        |                            currentC.send_ehloStringToS((new SMTPMessage("EHLO", payload7)).toString)
        |                            X1.breakable {
        |                              do {
        |                                X1Inner.breakable {
        |                                  {
        |                                    currentC.receive_CChoice1LabelFromS match {
        |                                      case Choice1._250DASH =>
        |                                        val payload8: String = currentC.receive_250dashStringFromS
        |                                        X1Inner.break()
        |                                      case Choice1._250 =>
        |                                        val payload9: String = currentC.receive_250StringFromS
        |                                        Y.breakable {
        |                                          do {
        |                                            YInner.breakable {
        |                                              {
        |                                                System.out.print("Choose a label among AUTH or QUIT: ")
        |                                                val label4: Int = if (safeRead(readerC) == "AUTH") {
        |                                                  1
        |                                                }
        |                                                else {
        |                                                  2
        |                                                }
        |                                                label4 match {
        |                                                  case 1 =>
        |                                                    currentC.send_AUTHToS()
        |                                                    System.out.print("Username: ")
        |                                                    val username: String = safeRead(readerC)
        |                                                    val console: Console = System.console
        |                                                    val tmp: Array[AnyRef] = Array()
        |                                                    val password: String = new String(console.readPassword("Password: ", tmp))
        |                                                    var token: String = ""
        |                                                    try {
        |                                                      val encoder: BASE64Encoder = new BASE64Encoder
        |                                                      token = encoder.encodeBuffer((username + "\u0000" + username + "\u0000" + password).getBytes("UTF-8")).trim
        |                                                    } catch {
        |                                                      case e: IOException =>
        |                                                        System.out.println("unable to use base64 encoding")
        |                                                    }
        |                                                    currentC.send_authStringToS((new SMTPMessage("AUTH PLAIN", token)).toString)
        |                                                    currentC.receive_CChoice2LabelFromS match {
        |                                                      case Choice2._235 =>
        |                                                        val payload11: String = currentC.receive_235StringFromS
        |                                                        Z1.breakable {
        |                                                          do {
        |                                                            Z1Inner.breakable {
        |                                                              {
        |                                                                System.out.print("Choose a label among MAIL or QUIT: ")
        |                                                                val label5: Int = if (safeRead(readerC) == "MAIL") {
        |                                                                  1
        |                                                                }
        |                                                                else {
        |                                                                  2
        |                                                                }
        |                                                                label5 match {
        |                                                                  case 1 =>
        |                                                                    currentC.send_MAILToS()
        |                                                                    System.out.print("Email from: ")
        |                                                                    val payload12: String = safeRead(readerC)
        |                                                                    currentC.send_mailStringToS((new SMTPMessage("MAIL FROM:<" + payload12 + ">")).toString)
        |                                                                    currentC.receive_CChoice3LabelFromS match {
        |                                                                      case Choice3._501 =>
        |                                                                        val payload13: String = currentC.receive_501StringFromS
        |                                                                        Z1Inner.break()
        |                                                                      case Choice3._250 =>
        |                                                                        val payload14: String = currentC.receive_250StringFromS
        |                                                                        System.out.println("Received from S: " + payload14)
        |                                                                        Z2.breakable {
        |                                                                          do {
        |                                                                            Z2Inner.breakable {
        |                                                                              {
        |                                                                                System.out.print("Choose a label among RCPT or DATA: ")
        |                                                                                val label6: Int = if (safeRead(readerC) == "RCPT") {
        |                                                                                  1
        |                                                                                }
        |                                                                                else {
        |                                                                                  2
        |                                                                                }
        |                                                                                label6 match {
        |                                                                                  case 1 =>
        |                                                                                    currentC.send_RCPTToS()
        |                                                                                    System.out.print("Send to S text for RCPT: ")
        |                                                                                    val payload15: String = safeRead(readerC)
        |                                                                                    currentC.send_rcptStringToS((new SMTPMessage("RCPT TO:<" + payload15 + ">")).toString)
        |                                                                                    currentC.receive_CChoice4LabelFromS match {
        |                                                                                      case Choice4._250 =>
        |                                                                                        val payload16: String = currentC.receive_250StringFromS
        |                                                                                        Z2Inner.break()
        |                                                                                    }
        |                                                                                    Z2.break()
        |                                                                                  case 2 =>
        |                                                                                    currentC.send_DATAToS()
        |                                                                                    currentC.send_dataStringToS((new SMTPMessage("DATA")).toString)
        |                                                                                    val payload18: String = currentC.receive_354StringFromS
        |                                                                                    System.out.println("Received from S: " + payload18)
        |                                                                                    do {
        |                                                                                      Z3Inner.breakable{
        |                                                                                        System.out.print("Choose a label among DATALINE, SUBJECT or ATAD: ")
        |                                                                                        safeRead(readerC) match {
        |                                                                                          case "DATALINE" =>
        |                                                                                            currentC.send_DATALINEToS()
        |                                                                                            System.out.print("Send to S text for DATALINE: ")
        |                                                                                            val payload19: String = safeRead(readerC)
        |                                                                                            currentC.send_datalineStringToS(payload19 + CRLF)
        |                                                                                            Z3Inner.break()
        |                                                                                          case "SUBJECT" =>
        |                                                                                            currentC.send_SUBJECTToS()
        |                                                                                            System.out.print("Send to S text for SUBJECT: ")
        |                                                                                            val payload20: String = safeRead(readerC)
        |                                                                                            currentC.send_subjectStringToS((new SMTPMessage("SUBJECT:" + payload20, CRLF)).toString)
        |                                                                                            Z3Inner.break()
        |                                                                                          case "ATAD" =>
        |                                                                                            currentC.send_ATADToS()
        |                                                                                            currentC.send_atadStringToS("." + CRLF)
        |                                                                                            val payload22: String = currentC.receive_250StringFromS
        |                                                                                            System.out.println("Received from S: " + payload22)
        |                                                                                            Z1Inner.break()
        |                                                                                        }
        |                                                                                      }
        |                                                                                    } while (true)
        |                                                                                }
        |                                                                              }
        |                                                                            }
        |                                                                          } while (true)
        |                                                                        }
        |                                                                        Z1.break()
        |                                                                    }
        |                                                                    Z1.break()
        |                                                                  case 2 =>
        |                                                                    currentC.send_QUITToS()
        |                                                                    val payload23: String = ""
        |                                                                    currentC.send_quitStringToS(payload23)
        |                                                                    Z1.break()
        |                                                                }
        |                                                              }
        |                                                            }
        |                                                          } while (true)
        |                                                        }
        |                                                        Y.break()
        |                                                      case Choice2._535 =>
        |                                                        val payload24: String = currentC.receive_535StringFromS
        |                                                        //System.out.println("Received from S: error " + payload24);
        |                                                        YInner.break()
        |                                                    }
        |                                                    Y.break()
        |                                                  case 2 =>
        |                                                    currentC.send_QUITToS()
        |                                                    val payload25: String = ""
        |                                                    currentC.send_quitStringToS(payload25)
        |                                                    Y.break()
        |                                                }
        |                                              }
        |                                            }
        |                                          } while (true)
        |                                        }
        |                                        X1.break()
        |                                    }
        |                                  }
        |                                }
        |                              } while (true)
        |                            }
        |                            X.break()
        |                          case 2 =>
        |                            currentC.send_QUITToS()
        |                            val payload26: String = ""
        |                            currentC.send_quitStringToS(payload26)
        |                            X.break()
        |                        }
        |                        X.break()
        |                      case "QUIT" =>
        |                        currentC.send_QUITToS()
        |                        val payload27: String = ""
        |                        currentC.send_quitStringToS(payload27)
        |                        X.break()
        |                   }
        |                    X.break()
        |               }
        |              }
        |            }
        |          } while (true)
        |        }
        |
        |      case "QUIT" =>
        |        currentC.send_QUITToS()
        |        val payload28: String = ""
        |        currentC.send_quitStringToS(payload28)
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "SMTPServer" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |object SChoice1 extends Enumeration {
        |  type SChoice1 = Value
        |  val EHLO, QUIT = Value
        |}
        |
        |object SChoice2 extends Enumeration {
        |  type SChoice2 = Value
        |  val STARTTLS, QUIT = Value
        |}
        |
        |object SChoice3 extends Enumeration {
        |  type SChoice3 = Value
        |  val AUTH, QUIT = Value
        |}
        |
        |object SChoice4 extends Enumeration {
        |  type SChoice4 = Value
        |  val MAIL, QUIT = Value
        |}
        |
        |object SChoice5 extends Enumeration {
        |  type SChoice5 = Value
        |  val RCPT, DATA = Value
        |}
        |
        |object SChoice6 extends Enumeration {
        |  type SChoice6 = Value
        |  val DATALINE, SUBJECT, ATAD = Value
        |}
        |
        |object SMTPMessage {
        |  def Parse(message: String): SMTPMessage = {
        |    val matches = message.split(" |-", 2)
        |    new SMTPMessage(matches(0), matches(1), message.charAt(3) == '-')
        |  }
        |}
        |
        |class SMTPMessage {
        |  private var command: String = null
        |  private var payload: String = null
        |  private var isDashed = false
        |
        |  def this(command: String, payload: String, isDashed: Boolean) {
        |    this()
        |    this.command = command
        |    this.payload = payload
        |    this.isDashed = isDashed
        |  }
        |
        |  def this(command: String, payload: String) {
        |    this()
        |    this.command = command
        |    this.payload = payload
        |    this.isDashed = false
        |  }
        |
        |  def this(command: String) {
        |    this()
        |    this.command = command
        |    this.payload = null
        |    this.isDashed = false
        |  }
        |
        |  override def toString: String = {
        |    var message: String = null
        |    if (this.payload == null) message = this.command + "\\r\\n"
        |    else message = this.command + " " + this.payload + "\\r\\n"
        |    message
        |  }
        |
        |  def getCommand: String = this.command
        |
        |  def getPayload: String = this.payload
        |
        |  def getIsDashed: Boolean = this.isDashed
        |}
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
        |import java.net.{ServerSocket, Socket}
        |
        |import compilerPlugin.SChoice1.EHLO
        |import compilerPlugin.SChoice2.STARTTLS
        |import compilerPlugin.SChoice3.AUTH
        |import compilerPlugin.SChoice4.MAIL
        |import compilerPlugin.SChoice5.RCPT
        |import compilerPlugin.SChoice6.DATALINE
        |
        |import scala.util.control.Breaks
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\SMTPSProtocol.scala") class SRole() { // Bind the sockets
        |  var serverC: ServerSocket = null
        |  // Connecting to the server
        |  try // Create the sockets
        |    serverC = new ServerSocket(20000)
        |  catch {
        |    case e: IOException =>
        |      System.out.println("Unable to listen on ports")
        |      System.exit(-1)
        |  }
        |  // Accept a client connection
        |  var socketC: Socket = null
        |  try {
        |    System.out.println("Accepting...")
        |    socketC = serverC.accept
        |    System.out.println("C accepted")
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Accept failed")
        |      System.exit(-1)
        |  }
        |  private var socketCIn: BufferedReader = null
        |  private var socketCOut: PrintWriter = null
        |  // Create the read and write streams
        |  try {
        |    socketCIn = new BufferedReader(new InputStreamReader(socketC.getInputStream))
        |    socketCOut = new PrintWriter(socketC.getOutputStream, true)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Read failed")
        |      System.exit(-1)
        |  }
        |
        |
        |  def send_220StringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |
        |  def receive_SChoice1LabelFromC(): SChoice1.Value = {
        |    var stringLabelSChoice1 = ""
        |    try stringLabelSChoice1 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice1 match {
        |      case "EHLO" =>
        |        SChoice1.EHLO
        |      case "QUIT" =>
        |        SChoice1.QUIT
        |      case _ =>
        |        SChoice1.QUIT
        |    }
        |  }
        |
        |  def receive_ehloStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_250DASHToC(): Unit = {
        |    this.socketCOut.println("_250DASH")
        |  }
        |
        |  def send_250ToC(): Unit = {
        |    this.socketCOut.println("_250")
        |  }
        |
        |  def send_250dashStringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |
        |  def send_250StringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |
        |  def receive_SChoice2LabelFromC(): SChoice2.Value = {
        |    var stringLabelSChoice2 = ""
        |    try stringLabelSChoice2 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice2 match {
        |      case "STARTTLS" =>
        |        SChoice2.STARTTLS
        |      case "QUIT" =>
        |        SChoice2.QUIT
        |      case _ =>
        |        SChoice2.QUIT
        |    }
        |  }
        |
        |  def receive_starttlsStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_SChoice3LabelFromC(): SChoice3.Value = {
        |    var stringLabelSChoice3 = ""
        |    try stringLabelSChoice3 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice3 match {
        |      case "AUTH" =>
        |        SChoice3.AUTH
        |      case "QUIT" =>
        |        SChoice3.QUIT
        |      case _ =>
        |        SChoice3.QUIT
        |    }
        |  }
        |
        |  def receive_authStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_235ToC(): Unit = {
        |    this.socketCOut.println("_235")
        |  }
        |
        |  def send_535ToC(): Unit = {
        |    this.socketCOut.println("_535")
        |  }
        |
        |  def send_235StringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |
        |  def receive_SChoice4LabelFromC(): SChoice4.Value = {
        |    var stringLabelSChoice4 = ""
        |    try stringLabelSChoice4 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice4 match {
        |      case "MAIL" =>
        |        SChoice4.MAIL
        |      case "QUIT" =>
        |        SChoice4.QUIT
        |      case _ =>
        |        SChoice4.QUIT
        |    }
        |  }
        |
        |  def receive_mailStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_501ToC(): Unit = {
        |    this.socketCOut.println("_501")
        |  }
        |
        |  def send_501StringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |
        |  def receive_SChoice5LabelFromC(): SChoice5.Value = {
        |    var stringLabelSChoice5 = ""
        |    try stringLabelSChoice5 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice5 match {
        |      case "RCPT" =>
        |        SChoice5.RCPT
        |      case "DATA" =>
        |        SChoice5.DATA
        |      case _ =>
        |        SChoice5.DATA
        |    }
        |  }
        |
        |  def receive_rcptStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_dataStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_354StringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |
        |  def receive_SChoice6LabelFromC(): SChoice6.Value = {
        |    var stringLabelSChoice6 = ""
        |    try stringLabelSChoice6 = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelSChoice6 match {
        |      case "DATALINE" =>
        |        SChoice6.DATALINE
        |      case "SUBJECT" =>
        |        SChoice6.SUBJECT
        |      case "ATAD" =>
        |        SChoice6.ATAD
        |      case _ =>
        |        SChoice6.ATAD
        |    }
        |  }
        |
        |  def receive_datalineStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_subjectStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_atadStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_quitStringFromC(): String = {
        |    var line = ""
        |    try line = this.socketCIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def send_535StringToC(payload0: String): Unit = {
        |    this.socketCOut.println(payload0)
        |  }
        |}
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader}
        |
        |
        |object SMain {
        |  def safeRead(readerS: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerS.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val X = new Breaks
        |    val XInner = new Breaks
        |    val X1 = new Breaks
        |    val X1Inner = new Breaks
        |    val Y = new Breaks
        |    val YInner = new Breaks
        |    val Z1 = new Breaks
        |    val Z1Inner = new Breaks
        |    val Z2 = new Breaks
        |    val Z2Inner = new Breaks
        |    val Z3 = new Breaks
        |    val Z3Inner = new Breaks
        |    val currentS = new SRole
        |    // readerS can be used to input strings, and then use them in send method invocation
        |    val readerS = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the S typestate
        |    System.out.print("Send to C: ")
        |    val payload1 = safeRead(readerS)
        |    currentS.send_220StringToC(payload1)
        |    currentS.receive_SChoice1LabelFromC match {
        |      case EHLO =>
        |        val payload3 = currentS.receive_ehloStringFromC
        |        System.out.println("Received from C: " + payload3)
        |        do {
        |          XInner.breakable {
        |            System.out.print("Choose a label among [_250DASH, _250]: ")
        |            val sread1 = safeRead(readerS)
        |            sread1 match {
        |              case "_250DASH" =>
        |                currentS.send_250DASHToC()
        |                System.out.print("Send to C: ")
        |                val payload5 = safeRead(readerS)
        |                currentS.send_250dashStringToC(payload5)
        |                XInner.break()
        |              case "_250" =>
        |                currentS.send_250ToC()
        |                System.out.print("Send to C: ")
        |                val payload7 = safeRead(readerS)
        |                currentS.send_250StringToC(payload7)
        |                currentS.receive_SChoice2LabelFromC match {
        |                  case STARTTLS =>
        |                    val payload9 = currentS.receive_starttlsStringFromC
        |                    System.out.println("Received from C: " + payload9)
        |                    System.out.print("Send to C: ")
        |                    val payload11 = safeRead(readerS)
        |                    currentS.send_220StringToC(payload11)
        |                    currentS.receive_SChoice1LabelFromC match {
        |                      case EHLO =>
        |                        val payload13 = currentS.receive_ehloStringFromC
        |                        System.out.println("Received from C: " + payload13)
        |                        X1.breakable {
        |                          do {
        |                            X1Inner.breakable {
        |                              System.out.print("Choose a label among [_250DASH, _250]: ")
        |                              val sread2 = safeRead(readerS)
        |                              sread2 match {
        |                                case "_250DASH" =>
        |                                  currentS.send_250DASHToC()
        |                                  System.out.print("Send to C: ")
        |                                  val payload15 = safeRead(readerS)
        |                                  currentS.send_250dashStringToC(payload15)
        |                                  X1Inner.break()
        |                                case "_250" =>
        |                                  currentS.send_250ToC()
        |                                  System.out.print("Send to C: ")
        |                                  val payload17 = safeRead(readerS)
        |                                  currentS.send_250StringToC(payload17)
        |                                  Y.breakable {
        |                                    do {
        |                                      YInner.breakable {
        |                                        currentS.receive_SChoice3LabelFromC match {
        |                                          case AUTH =>
        |                                            val payload19 = currentS.receive_authStringFromC
        |                                            System.out.println("Received from C: " + payload19)
        |                                            System.out.print("Choose a label among [_235, _535]: ")
        |                                            val sread3 = safeRead(readerS)
        |                                            sread3 match {
        |                                              case "_235" =>
        |                                                currentS.send_235ToC()
        |                                                System.out.print("Send to C: ")
        |                                                val payload21 = safeRead(readerS)
        |                                                currentS.send_235StringToC(payload21)
        |                                                Z1.breakable {
        |                                                  do {
        |                                                    Z1Inner.breakable {
        |                                                      currentS.receive_SChoice4LabelFromC match {
        |                                                        case MAIL =>
        |                                                          val payload23 = currentS.receive_mailStringFromC
        |                                                          System.out.println("Received from C: " + payload23)
        |                                                          System.out.print("Choose a label among [_501, _250]: ")
        |                                                          val sread4 = safeRead(readerS)
        |                                                          sread4 match {
        |                                                            case "_501" =>
        |                                                              currentS.send_501ToC()
        |                                                              System.out.print("Send to C: ")
        |                                                              val payload25 = safeRead(readerS)
        |                                                              currentS.send_501StringToC(payload25)
        |                                                              Z1Inner.break()
        |
        |                                                            case "_250" =>
        |                                                              currentS.send_250ToC()
        |                                                              System.out.print("Send to C: ")
        |                                                              val payload27 = safeRead(readerS)
        |                                                              currentS.send_250StringToC(payload27)
        |                                                              Z2.breakable {
        |                                                                do {
        |                                                                  Z2Inner.breakable {
        |                                                                    currentS.receive_SChoice5LabelFromC match {
        |                                                                      case RCPT =>
        |                                                                        val payload29 = currentS.receive_rcptStringFromC
        |                                                                        System.out.println("Received from C: " + payload29)
        |                                                                        System.out.print("Choose a label among [_250]: ")
        |                                                                        val sread5 = safeRead(readerS)
        |                                                                        sread5 match {
        |                                                                          case "_250" =>
        |                                                                            currentS.send_250ToC()
        |                                                                            System.out.print("Send to C: ")
        |                                                                            val payload31 = safeRead(readerS)
        |                                                                            currentS.send_250StringToC(payload31)
        |                                                                            Z2Inner.break()
        |
        |                                                                        }
        |                                                                        Z2.break()
        |
        |                                                                      case SChoice5.DATA =>
        |                                                                        val payload33 = currentS.receive_dataStringFromC
        |                                                                        System.out.println("Received from C: " + payload33)
        |                                                                        System.out.print("Send to C: ")
        |                                                                        val payload35 = safeRead(readerS)
        |                                                                        currentS.send_354StringToC(payload35)
        |                                                                        do {
        |                                                                          Z3Inner.breakable {
        |                                                                            currentS.receive_SChoice6LabelFromC() match {
        |                                                                              case DATALINE =>
        |                                                                                val payload37 = currentS.receive_datalineStringFromC
        |                                                                                System.out.println("Received from C: " + payload37)
        |                                                                                Z3Inner.break()
        |
        |                                                                              case SChoice6.SUBJECT =>
        |                                                                                val payload39 = currentS.receive_subjectStringFromC
        |                                                                                System.out.println("Received from C: " + payload39)
        |                                                                                Z3Inner.break()
        |
        |                                                                              case SChoice6.ATAD =>
        |                                                                                val payload41 = currentS.receive_atadStringFromC
        |                                                                                System.out.println("Received from C: " + payload41)
        |                                                                                System.out.print("Send to C: ")
        |                                                                                val payload43 = safeRead(readerS)
        |                                                                                currentS.send_250StringToC(payload43)
        |                                                                                Z1Inner.break()
        |                                                                            }
        |                                                                          }
        |                                                                        } while (true)
        |                                                                    }
        |                                                                  }
        |                                                                } while (true)
        |                                                              }
        |                                                          }
        |                                                          Z1.break()
        |                                                        case SChoice4.QUIT =>
        |                                                          val payload45 = currentS.receive_quitStringFromC
        |                                                          System.out.println("Received from C: " + payload45)
        |                                                          Z1.break()
        |                                                      }
        |                                                    }
        |                                                  } while (true)
        |                                                }
        |                                                Y.break()
        |
        |                                              case "_535" =>
        |                                                currentS.send_535ToC()
        |                                                System.out.print("Send to C: ")
        |                                                val payload47 = safeRead(readerS)
        |                                                currentS.send_535StringToC(payload47)
        |                                                YInner.break()
        |
        |                                            }
        |                                            Y.break()
        |
        |                                          case SChoice3.QUIT =>
        |                                            val payload49 = currentS.receive_quitStringFromC
        |                                            System.out.println("Received from C: " + payload49)
        |                                            Y.break()
        |
        |                                        }
        |                                      }
        |                                    } while (true)
        |                                  }
        |                                  X1.break()
        |
        |                              }
        |                            }
        |                          } while (true)
        |                        }
        |                        X.break()
        |
        |                      case SChoice1.QUIT =>
        |                        val payload51 = currentS.receive_quitStringFromC
        |                        System.out.println("Received from C: " + payload51)
        |                        X.break()
        |
        |                    }
        |                    X.break()
        |
        |                  case SChoice2.QUIT =>
        |                    val payload53 = currentS.receive_quitStringFromC
        |                    System.out.println("Received from C: " + payload53)
        |                    X.break()
        |
        |                }
        |                X.break()
        |
        |            }
        |          }
        |        } while (true)
        |
        |      case SChoice1.QUIT =>
        |        val payload55 = currentS.receive_quitStringFromC
        |        System.out.println("Received from C: " + payload55)
        |
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region stack
  "stack used in stackUser example 1" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |
        |
        |object Stack {
        |  def main(args: Array[String]): Unit = {
        |    val loop = new Breaks
        |    val loopInner = new Breaks
        |    val s = new Stack
        |    s.initialise(0)
        |    s.put(new Node(0))
        |    s.put(new Node(1))
        |    var i = 2
        |    do s.put(new Node({
        |      i += 1; i - 1
        |    })) while ( {
        |      i < 42
        |    })
        |    loop.breakable {
        |      do {
        |        loopInner.breakable {
        |          val n = s.get
        |          System.out.println(n.get)
        |          s.isEmpty match {
        |            case true =>
        |              loop.break()
        |            case false =>
        |              loopInner.break()
        |          }
        |        }
        |      } while (true)
        |    }
        |    s.close()
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\CollectionProtocol.scala")
        |class Stack() {
        |  private var head:Node = null
        |
        |  def initialise(i: Int): Unit = {
        |    head = null
        |  }
        |
        |  def put(n: Node): Unit = {
        |    head = n.next(head)
        |  }
        |
        |  def get(): Node = {
        |    val tmp = head
        |    head = head.getNext()
        |    tmp
        |  }
        |
        |  def isEmpty(): Boolean = {
        |    if (head == null) return true
        |    false
        |  }
        |
        |  def close(): Unit = {
        |  }
        |}
        |
        |
        |class Node(var i: Int) {
        |  var next:Node = null
        |
        |  def set(i: Int): Unit = {
        |    this.i = i
        |  }
        |
        |  def get(): String = "The number of this Node is: " + i
        |
        |  def next(in: Node): Node = {
        |    next = in
        |    this
        |  }
        |
        |  def getNext(): Node = {
        |    this.next
        |  }
        |}
        |
        |object StackUser {
        |  def main(args: Array[String]): Unit = {
        |    var s = new Stack
        |    s.initialise(0)
        |    val su = new StackUser
        |    s = su.produce(s)
        |    s = su.produce(s)
        |    s = su.consume(s)
        |    s = su.produce(s, 60)
        |    s = su.consume(s)
        |    s.close
        |    su.close()
        |  }
        |}
        |
        |class StackUser {
        |  def produce(s: Stack): Stack = {
        |    s.put(new Node(0))
        |    s.put(new Node(1))
        |    s.put(new Node(2))
        |    s.put(new Node(3))
        |    s.put(new Node(4))
        |    s
        |  }
        |
        |  def produce(s: Stack, j: Int): Stack = {
        |    var i = 0
        |    do s.put(new Node({
        |      i += 1; i - 1
        |    })) while ( {
        |      i < j
        |    })
        |    s
        |  }
        |
        |  def consume(s: Stack): Stack = {
        |    val loop = new Breaks
        |    val loopInner = new Breaks
        |    loop.breakable {
        |      do {
        |        loopInner.breakable {
        |          System.out.println(s.get.get + " ")
        |          s.isEmpty match {
        |            case true =>
        |              loop.break()
        |            case false =>
        |              loopInner.break()
        |          }
        |        }
        |      } while (true)
        |    }
        |    s
        |  }
        |
        |  def close(): Unit = {
        |  }
        |}
        |
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "stack used in stackUser example 1 with an added protocol" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |
        |
        |object Stack {
        |  def main(args: Array[String]): Unit = {
        |    val loop = new Breaks
        |    val loopInner = new Breaks
        |    val s = new Stack
        |    s.initialise(0)
        |    s.put(new Node(0))
        |    s.put(new Node(1))
        |    var i = 2
        |    do s.put(new Node({
        |      i += 1; i - 1
        |    })) while ( {
        |      i < 42
        |    })
        |    loop.breakable {
        |      do {
        |        loopInner.breakable {
        |          val n = s.get
        |          System.out.println(n.get)
        |          s.isEmpty match {
        |            case true =>
        |              loop.break()
        |            case false =>
        |              loopInner.break()
        |          }
        |        }
        |      } while (true)
        |    }
        |    s.close()
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\CollectionProtocol.scala")
        |class Stack() {
        |  private var head:Node = null
        |
        |  def initialise(i: Int): Unit = {
        |    head = null
        |  }
        |
        |  def put(n: Node): Unit = {
        |    head = n.next(head)
        |  }
        |
        |  def get(): Node = {
        |    val tmp = head
        |    head = head.getNext()
        |    tmp
        |  }
        |
        |  def isEmpty(): Boolean = {
        |    if (head == null) return true
        |    false
        |  }
        |
        |  def close(): Unit = {
        |  }
        |}
        |
        |
        |class Node(var i: Int) {
        |  var next:Node = null
        |
        |  def set(i: Int): Unit = {
        |    this.i = i
        |  }
        |
        |  def get(): String = "The number of this Node is: " + i
        |
        |  def next(in: Node): Node = {
        |    next = in
        |    this
        |  }
        |
        |  def getNext(): Node = {
        |    this.next
        |  }
        |}
        |
        |object StackUser {
        |  def main(args: Array[String]): Unit = {
        |    var s = new Stack
        |    s.initialise(0)
        |    val su = new StackUser
        |    s = su.produce(s)
        |    s = su.produce(s)
        |    s = su.consume(s)
        |    s = su.produce(s, 60)
        |    s = su.consume(s)
        |    s.close
        |    su.close()
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\StackUserProtocol.scala")
        |class StackUser {
        |  def produce(s: Stack): Stack = {
        |    s.put(new Node(0))
        |    s.put(new Node(1))
        |    s.put(new Node(2))
        |    s.put(new Node(3))
        |    s.put(new Node(4))
        |    s
        |  }
        |
        |  def produce(s: Stack, j: Int): Stack = {
        |    var i = 0
        |    do s.put(new Node({
        |      i += 1; i - 1
        |    })) while ( {
        |      i < j
        |    })
        |    s
        |  }
        |
        |  def consume(s: Stack): Stack = {
        |    val loop = new Breaks
        |    val loopInner = new Breaks
        |    loop.breakable {
        |      do {
        |        loopInner.breakable {
        |          System.out.println(s.get.get + " ")
        |          s.isEmpty match {
        |            case true =>
        |              loop.break()
        |            case false =>
        |              loopInner.break()
        |          }
        |        }
        |      } while (true)
        |    }
        |    s
        |  }
        |
        |  def close(): Unit = {
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "stack used in stackUser example 2 with an added protocol" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |
        |
        |object Stack {
        |  def main(args: Array[String]): Unit = {
        |    val loop = new Breaks
        |    val loopInner = new Breaks
        |    val s = new Stack
        |    s.initialise(0)
        |    s.put(new Node(0))
        |    s.put(new Node(1))
        |    var i = 2
        |    do s.put(new Node({
        |      i += 1;
        |      i - 1
        |    })) while ( {
        |      i < 42
        |    })
        |    loop.breakable {
        |      do {
        |        loopInner.breakable {
        |          val n = s.get
        |          System.out.println(n.get)
        |          s.isEmpty match {
        |            case true =>
        |              loop.break()
        |            case false =>
        |              loopInner.break()
        |          }
        |        }
        |      } while (true)
        |    }
        |    s.close()
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\CollectionProtocol.scala")
        |class Stack() {
        |  private var head: Node = null
        |
        |  def initialise(i: Int): Unit = {
        |    head = null
        |  }
        |
        |  def put(n: Node): Unit = {
        |    head = n.next(head)
        |  }
        |
        |  def get(): Node = {
        |    val tmp = head
        |    head = head.getNext()
        |    tmp
        |  }
        |
        |  def isEmpty(): Boolean = {
        |    if (head == null) return true
        |    false
        |  }
        |
        |  def close(): Unit = {
        |  }
        |}
        |
        |
        |class Node(var i: Int) {
        |  var next: Node = null
        |
        |  def set(i: Int): Unit = {
        |    this.i = i
        |  }
        |
        |  def get(): String = "The number of this Node is: " + i
        |
        |  def next(in: Node): Node = {
        |    next = in
        |    this
        |  }
        |
        |  def getNext(): Node = {
        |    this.next
        |  }
        |}
        |
        |
        |object StackUser {
        |  def main(args: Array[String]): Unit = {
        |    val su = new StackUser
        |    su.produce(20)
        |    su.consume()
        |    su.produce()
        |    su.produce()
        |    su.consume()
        |    su.close()
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\StackUserProtocol2.scala")
        |class StackUser() {
        |  var s = new Stack
        |  s.initialise(0)
        |
        |  def produce(): Unit = {
        |    s.put(new Node(0))
        |    s.put(new Node(1))
        |  }
        |
        |  def produce(j: Int): Unit = {
        |    var i = 0
        |    do s.put(new Node({
        |      i += 1;
        |      i - 1
        |    })) while ( {
        |      i < j
        |    })
        |  }
        |
        |  def consume(): Unit = {
        |    val loop = new Breaks
        |    val loopInner = new Breaks
        |    loop.breakable {
        |      do {
        |        loopInner.breakable {
        |          System.out.println(s.get.get + " ")
        |          s.isEmpty match {
        |            case true =>
        |              loop.break()
        |            case false =>
        |              loopInner.break()
        |          }
        |        }
        |      }
        |      while ( {
        |        true
        |      })
        |    }
        |  }
        |
        |  def close(): Unit = {
        |    s.close
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region file
  "file" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import compilerPlugin.Status.OK
        |
        |import scala.util.control.Breaks
        |
        |
        |object Status extends Enumeration {
        |  type Status = Value
        |  val OK, ERROR = Value
        |}
        |
        |
        |object File {
        |  def main(args: Array[String]): Unit = {
        |    val myFile = new File("file.txt")
        |    val a = myFile
        |    processFile(a)
        |  }
        |
        |  def processFile(myFile: File): Unit = {
        |    myFile.open match {
        |      case OK =>
        |        if (true) {
        |          val loop = new Breaks
        |          loop.breakable {
        |            while (true) {
        |              myFile.hasNext match {
        |                case true =>
        |                  myFile.read()
        |
        |                case false =>
        |                  loop.break()
        |              }
        |            }
        |          }
        |        }
        |        myFile.close()
        |
        |      case Status.ERROR =>
        |        System.out.println("File <file.txt> not found!")
        |
        |    }
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\FileProtocol.scala")
        |class File(var file: String) {
        |  protected var reader: MyBufferedReader = null
        |  private var readBuffer: Array[Char] = null
        |  private var i = 0
        |  reader = new MyBufferedReader(file)
        |  readBuffer = new Array[Char](1024)
        |  i = 0
        |
        |
        |  def open(): Status.Value = {
        |    if (reader.open) return Status.OK
        |    Status.ERROR
        |  }
        |
        |  def close(): Unit = {
        |    reader.close()
        |  }
        |
        |  //The next two methods demonstrate that
        |  // a created typestate object can
        |  // be assigned in a linear way and
        |  // passed around as an argument
        |  def hasNext(): Boolean = {
        |    if (reader.ready) return true
        |    false
        |  }
        |
        |  def read(): Unit = {
        |    readBuffer({
        |      i += 1;
        |      i - 1
        |    }) = reader.read
        |  }
        |}
        |
        |import java.io.BufferedReader
        |import java.io.FileNotFoundException
        |import java.io.FileReader
        |import java.io.IOException
        |
        |class MyBufferedReader(var file: String) {
        |  private var reader: BufferedReader = null
        |
        |  def open: Boolean = {
        |    try reader = new BufferedReader(new FileReader(file))
        |    catch {
        |      case e: FileNotFoundException =>
        |        return false
        |    }
        |    true
        |  }
        |
        |  def close(): Unit = {
        |    try reader.close()
        |    catch {
        |      case e: IOException =>
        |        e.printStackTrace()
        |        System.exit(-1)
        |    }
        |  }
        |
        |  def ready: Boolean = {
        |    try if (reader.ready) return true
        |    catch {
        |      case e: IOException =>
        |        return false
        |    }
        |    false
        |  }
        |
        |  def read: Char = {
        |    var c = -1
        |    try c = reader.read
        |    catch {
        |      case e: IOException =>
        |        e.printStackTrace()
        |        System.exit(-1)
        |    }
        |    c.toChar
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region http
  "http" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |
        |
        |object Choice1 extends Enumeration {
        |  type Choice1 = Value
        |  val _200, _404 = Value
        |}
        |
        |object Choice2 extends Enumeration {
        |  type Choice2 = Value
        |  val DATE, SERVER, STRICTTS, LASTM, ETAG, ACCEPTR, CONTENTL, VARY, CONTENTT, VIA, CACHEC, P3P, XXSSPROTECTION, XFRAMEOPT, SETCOOKIE, TRANSFERE, EXPIRES, BODY = Value
        |}
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader, PrintWriter}
        |import java.net.{InetAddress, ServerSocket, Socket}
        |
        |
        |object CRole {
        |  def getString(socketSIn: BufferedReader): String = {
        |    var body = ""
        |    try {
        |      var i = 0
        |      while ( {
        |        i != -1 && i.toChar != '\r' && i.toChar != '\n'
        |      }) {
        |        i = socketSIn.read
        |        body += i.toChar
        |      }
        |    } catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    body
        |  }
        |}
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\CProtocol.scala") class CRole() { // Bind the sockets
        |  private var socketSIn: BufferedReader = _
        |  private var socketSOut: PrintWriter = _
        |  val serverS: ServerSocket = null
        |  // Connecting to the server
        |  var addr: InetAddress = _
        |  var socket: Socket = _
        |  try { // Create the sockets
        |    addr = InetAddress.getByName("www.ecosia.com")
        |    socket = new Socket(addr, 80)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Unable to listen on ports")
        |      System.exit(-1)
        |  }
        |  // Accept a client connection
        |  // Create the read and write streams
        |  try {
        |    socketSIn = new BufferedReader(new InputStreamReader(socket.getInputStream))
        |    socketSOut = new PrintWriter(socket.getOutputStream, true)
        |  } catch {
        |    case e: IOException =>
        |      System.out.println("Read failed")
        |      System.exit(-1)
        |  }
        |
        |
        |  def send_REQUESTToS(): Unit = {
        |    this.socketSOut.print("")
        |  }
        |
        |  def send_requestStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_HOSTToS(): Unit = {
        |    this.socketSOut.print("Host: ")
        |  }
        |
        |  def send_USERAToS(): Unit = {
        |    this.socketSOut.print("User-Agent: ")
        |  }
        |
        |  def send_ACCEPTTToS(): Unit = {
        |    this.socketSOut.print("Accept: ")
        |  }
        |
        |  def send_ACCEPTLToS(): Unit = {
        |    this.socketSOut.print("Accept-Language: ")
        |  }
        |
        |  def send_ACCEPTEToS(): Unit = {
        |    this.socketSOut.print("Accept-Encoding: ")
        |  }
        |
        |  def send_DNTToS(): Unit = {
        |    this.socketSOut.print("DNT: ")
        |  }
        |
        |  def send_CONNECTIONToS(): Unit = {
        |    this.socketSOut.print("Connection: ")
        |  }
        |
        |  def send_UPGRADEIRToS(): Unit = {
        |    this.socketSOut.print("Upgrade-Insecure-Requests: ")
        |  }
        |
        |  def send_COOKIEToS(): Unit = {
        |    this.socketSOut.print("Cookie: ")
        |  }
        |
        |  def send_BODYToS(): Unit = {
        |    this.socketSOut.println(" ")
        |  }
        |
        |  def send_hostStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_userAStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_acceptTStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_acceptLStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_acceptEStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_DNTIntToS(payload: Integer): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_connectionStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_upgradeIRStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_cookieStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload)
        |  }
        |
        |  def send_bodyStrToS(payload: String): Unit = {
        |    this.socketSOut.println(payload + "\r\n")
        |  }
        |
        |  def receive_httpvStrFromS(): String = {
        |    var line = ""
        |    try {
        |      var i = 0
        |      while ( {
        |        i != -1 && i.toChar != ' '
        |      }) {
        |        i = socketSIn.read
        |        line += i.toChar
        |      }
        |    } catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error." + e)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_Choice1LabelFromS(): Choice1.Value = {
        |    var stringLabelChoice1 = ""
        |    try {
        |      var i = 0
        |      while ( {
        |        i != -1 && i.toChar != ' '
        |      }) {
        |        i = socketSIn.read
        |        stringLabelChoice1 += i.toChar
        |      }
        |    } catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error." + e)
        |        System.exit(-1)
        |    }
        |    stringLabelChoice1.trim match {
        |      case "200" =>
        |        Choice1._200
        |      case "404" =>
        |        Choice1._404
        |      case _ =>
        |        Choice1._404
        |    }
        |  }
        |
        |  def receive_200StrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_404StrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_Choice2LabelFromS(): Choice2.Value = {
        |    var stringLabelChoice2 = ""
        |    try {
        |      var i = 0
        |      while ( {
        |        i != -1 && i.toChar != ' '
        |      }) {
        |        i = socketSIn.read
        |        stringLabelChoice2 += i.toChar
        |      }
        |    } catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error, unable to get label" + e.getMessage)
        |        System.exit(-1)
        |    }
        |    stringLabelChoice2.trim match {
        |      case "Date:" =>
        |        Choice2.DATE
        |      case "Server:" =>
        |        Choice2.SERVER
        |      case "Strict-Transport-Security:" =>
        |        Choice2.STRICTTS
        |      case "Last-Modified:" =>
        |        Choice2.LASTM
        |      case "ETag:" =>
        |        Choice2.ETAG
        |      case "Accept-Ranges:" =>
        |        Choice2.ACCEPTR
        |      case "Content-Length" =>
        |        Choice2.CONTENTL
        |      case "Vary:" =>
        |        Choice2.VARY
        |      case "Content-Type:" =>
        |        Choice2.CONTENTT
        |      case "Via:" =>
        |        Choice2.VIA
        |      case "Cache-Control:" =>
        |        Choice2.CACHEC
        |      case "P3P:" =>
        |        Choice2.P3P
        |      case "X-XSS-Protection:" =>
        |        Choice2.XXSSPROTECTION
        |      case "X-Frame-Options:" =>
        |        Choice2.XFRAMEOPT
        |      case "Set-Cookie:" =>
        |        Choice2.SETCOOKIE
        |      case "Transfer-Encoding:" =>
        |        Choice2.TRANSFERE
        |      case "Expires:" =>
        |        Choice2.EXPIRES
        |      case "\r\n" =>
        |        Choice2.BODY
        |      case _ =>
        |        Choice2.BODY
        |    }
        |  }
        |
        |  def receive_dateStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_serverStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_strictTSStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_lastMStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_eTagStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_acceptRStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_contentLIntFromS(): Integer = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line.toInt
        |  }
        |
        |  def receive_varyStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_contentTStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_viaStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_cacheCStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_p3pStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_xxssProtectionStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_xframeOptStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_setCookieStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_transferEStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_expiresStrFromS(): String = {
        |    var line = ""
        |    try line = this.socketSIn.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Outpur error. " + e.getMessage)
        |        System.exit(-1)
        |    }
        |    line
        |  }
        |
        |  def receive_bodyStrFromS(): String = CRole.getString(socketSIn)
        |}
        |
        |
        |import java.io.{BufferedReader, IOException, InputStreamReader}
        |
        |
        |object CMain {
        |  def safeRead(readerC: BufferedReader): String = {
        |    var readline = ""
        |    try readline = readerC.readLine
        |    catch {
        |      case e: IOException =>
        |        System.out.println("Input/Output error, unable to read")
        |        System.exit(-1)
        |    }
        |    readline
        |  }
        |
        |  def main(args: Array[String]): Unit = { // Create the current role
        |    val Y = new Breaks
        |    val YInner = new Breaks
        |    val X = new Breaks
        |    val XInner = new Breaks
        |    val currentC = new CRole
        |    // readerC can be used to input strings, and then use them in send method invocation
        |    val readerC = new BufferedReader(new InputStreamReader(System.in))
        |    // Method invocation follows the C typestate
        |    System.out.print("Choose a label among [REQUEST]: ")
        |    val sread1 = safeRead(readerC)
        |    sread1 match {
        |      case "REQUEST" =>
        |        currentC.send_REQUESTToS()
        |        System.out.print("Send to S: ")
        |        val payload1 = safeRead(readerC)
        |        currentC.send_requestStrToS(payload1)
        |        X.breakable {
        |          do {
        |            XInner.breakable {
        |              System.out.print("Choose a label among [HOST, USERA, ACCEPTT, ACCEPTL, ACCEPTE, DNT, CONNECTION, UPGRADEIR, COOKIE, BODY]: ")
        |              val sread2 = safeRead(readerC)
        |              sread2 match {
        |                case "HOST" =>
        |                  currentC.send_HOSTToS()
        |                  System.out.print("Send to S: ")
        |                  val payload2 = safeRead(readerC)
        |                  currentC.send_hostStrToS(payload2)
        |                  XInner.break()
        |
        |                case "USERA" =>
        |                  currentC.send_USERAToS()
        |                  System.out.print("Send to S: ")
        |                  val payload3 = safeRead(readerC)
        |                  currentC.send_userAStrToS(payload3)
        |                  XInner.break()
        |
        |                case "ACCEPTT" =>
        |                  currentC.send_ACCEPTTToS()
        |                  System.out.print("Send to S: ")
        |                  val payload4 = safeRead(readerC)
        |                  currentC.send_acceptTStrToS(payload4)
        |                  XInner.break()
        |
        |                case "ACCEPTL" =>
        |                  currentC.send_ACCEPTLToS()
        |                  System.out.print("Send to S: ")
        |                  val payload5 = safeRead(readerC)
        |                  currentC.send_acceptLStrToS(payload5)
        |                  XInner.break()
        |
        |                case "ACCEPTE" =>
        |                  currentC.send_ACCEPTEToS()
        |                  System.out.print("Send to S: ")
        |                  val payload6 = safeRead(readerC)
        |                  currentC.send_acceptEStrToS(payload6)
        |                  XInner.break()
        |
        |                case "DNT" =>
        |                  currentC.send_DNTToS()
        |                  System.out.print("Send to S: ")
        |                  val payload7 = safeRead(readerC).toInt
        |                  currentC.send_DNTIntToS(payload7)
        |                  XInner.break()
        |
        |                case "CONNECTION" =>
        |                  currentC.send_CONNECTIONToS()
        |                  System.out.print("Send to S: ")
        |                  val payload8 = safeRead(readerC)
        |                  currentC.send_connectionStrToS(payload8)
        |                  XInner.break()
        |
        |                case "UPGRADEIR" =>
        |                  currentC.send_UPGRADEIRToS()
        |                  System.out.print("Send to S: ")
        |                  val payload9 = safeRead(readerC)
        |                  currentC.send_upgradeIRStrToS(payload9)
        |                  XInner.break()
        |
        |                case "COOKIE" =>
        |                  currentC.send_COOKIEToS()
        |                  System.out.print("Send to S: ")
        |                  val payload10 = safeRead(readerC)
        |                  currentC.send_cookieStrToS(payload10)
        |                  XInner.break()
        |
        |                case "BODY" =>
        |                  currentC.send_BODYToS()
        |                  System.out.print("Send to S: ")
        |                  val payload11 = safeRead(readerC)
        |                  currentC.send_bodyStrToS(payload11)
        |                  X.break()
        |
        |              }
        |            }
        |          }
        |          while ( {
        |            true
        |          })
        |        }
        |
        |    }
        |    val payload12 = currentC.receive_httpvStrFromS()
        |    System.out.println("Received HTTPV from S: " + payload12)
        |    currentC.receive_Choice1LabelFromS() match {
        |      case Choice1._200 =>
        |        val payload13 = currentC.receive_200StrFromS()
        |        System.out.println("Received 200 from S: " + payload13)
        |      case Choice1._404 =>
        |        val payload14 = currentC.receive_404StrFromS()
        |        System.out.println("Received 404 from S: " + payload14)
        |    }
        |    Y.breakable {
        |      do {
        |        YInner.breakable {
        |          currentC.receive_Choice2LabelFromS() match {
        |            case Choice2.DATE =>
        |              val payload15 = currentC.receive_dateStrFromS()
        |              System.out.println("Received Date from S: " + payload15)
        |              YInner.break()
        |            case Choice2.SERVER =>
        |              val payload16 = currentC.receive_serverStrFromS()
        |              System.out.println("Received Server from S: " + payload16)
        |              YInner.break()
        |            case Choice2.STRICTTS =>
        |              val payload17 = currentC.receive_strictTSStrFromS()
        |              System.out.println("Received Strict-Transport-Security from S: " + payload17)
        |              YInner.break()
        |            case Choice2.LASTM =>
        |              val payload18 = currentC.receive_lastMStrFromS()
        |              System.out.println("Received Last-Modified from S: " + payload18)
        |              YInner.break()
        |            case Choice2.ETAG =>
        |              val payload19 = currentC.receive_eTagStrFromS()
        |              System.out.println("Received ETag from S: " + payload19)
        |              YInner.break()
        |            case Choice2.ACCEPTR =>
        |              val payload20 = currentC.receive_acceptRStrFromS()
        |              System.out.println("Received Accept-Ranges from S: " + payload20)
        |              YInner.break()
        |            case Choice2.CONTENTL =>
        |              val payload21 = currentC.receive_contentLIntFromS()
        |              System.out.println("Received Content-Length from S: " + payload21)
        |              YInner.break()
        |            case Choice2.VARY =>
        |              val payload22 = currentC.receive_varyStrFromS()
        |              System.out.println("Received Vary from S: " + payload22)
        |              YInner.break()
        |            case Choice2.CONTENTT =>
        |              val payload23 = currentC.receive_contentTStrFromS()
        |              System.out.println("Received Content-Type from S: " + payload23)
        |              YInner.break()
        |            case Choice2.VIA =>
        |              val payload24 = currentC.receive_viaStrFromS()
        |              System.out.println("Received Via from S: " + payload24)
        |              YInner.break()
        |            case Choice2.CACHEC =>
        |              val payload25 = currentC.receive_cacheCStrFromS()
        |              System.out.println("Received Cache-Control from S: " + payload25)
        |              YInner.break()
        |            case Choice2.P3P =>
        |              val payload26 = currentC.receive_p3pStrFromS()
        |              System.out.println("Received P3P from S: " + payload26)
        |              YInner.break()
        |            case Choice2.XXSSPROTECTION =>
        |              val payload27 = currentC.receive_xxssProtectionStrFromS()
        |              System.out.println("Received X-XSS-Protection from S: " + payload27)
        |              YInner.break()
        |            case Choice2.XFRAMEOPT =>
        |              val payload28 = currentC.receive_xframeOptStrFromS()
        |              System.out.println("Received X-Frame-Options from S: " + payload28)
        |              YInner.break()
        |            case Choice2.SETCOOKIE =>
        |              val payload29 = currentC.receive_setCookieStrFromS()
        |              System.out.println("Received Set-Cookie from S: " + payload29)
        |              YInner.break()
        |            case Choice2.TRANSFERE =>
        |              val payload30 = currentC.receive_transferEStrFromS()
        |              System.out.println("Received Transfer-Encoding from S: " + payload30)
        |              YInner.break()
        |            case Choice2.EXPIRES =>
        |              val payload31 = currentC.receive_expiresStrFromS()
        |              System.out.println("Received Expires from S: " + payload31)
        |              YInner.break()
        |            case Choice2.BODY =>
        |              val payload32 = currentC.receive_bodyStrFromS()
        |              System.out.println("Received Body from S: " + payload32)
        |              Y.break()
        |          }
        |        }
        |      }
        |      while (true)
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region iterator
  "iterator" should "not throw an exception" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |import scala.collection.mutable
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\StateIteratorProtocol.scala")
        |class StateIterator(var iter: Iterator[_]) {
        |  def next(): Any = {
        |    iter.next()
        |  }
        |
        |  def hasNext(): Boolean = {
        |    if (iter.hasNext) return true
        |    false
        |  }
        |
        |  def remove(): Unit = {
        |    iter.drop(1)
        |  }
        |}
        |
        |
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    val c = new mutable.HashSet[Integer]()
        |    var i:Integer = 0
        |    while ( {
        |      i < 32
        |    }) c.add({
        |      i += 1;
        |      i - 1
        |    })
        |    val iter = new StateIterator(c.iterator)
        |    val iterate = new Breaks
        |    val iterateInner = new Breaks
        |    iterate.breakable {
        |      do {
        |        iterateInner.breakable {
        |          iter.hasNext match {
        |            case true =>
        |              System.out.println({i = iter.next.asInstanceOf[Integer]})
        |              if (i % 2 == 0) iter.remove()
        |              iterateInner.break()
        |            case false =>
        |              iterate.break()
        |          }
        |        }
        |      } while ( {
        |        true
        |      })
        |    }
        |  }
        |}
        |""".stripMargin
    noException should be thrownBy{
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region loop
  "loop1" should "not throw an exception if an instance does not violates its protocol from a method with boolean return values" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\Loop.scala") class LoopImpl {
        |  def finished(): Boolean = true
        |}
        |
        |
        |object ClientTest1 extends App{
        |  def test(): Unit = {
        |    val loop = new LoopImpl
        |    val out = new Breaks
        |    val outInner = new Breaks
        |    out.breakable {
        |      do {
        |        outInner.breakable {
        |          loop.finished match {
        |            case false =>
        |              outInner.break()
        |            case true =>
        |          }
        |        }
        |      } while ( {
        |        false
        |      })
        |    }
        |  }
        |  test()
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "loop2" should "not throw an exception if an instance does not violates its protocol from a method with boolean return values" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename:String) extends scala.annotation.StaticAnnotation
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\Loop.scala")
        |class LoopImpl {
        |  def finished(): Boolean = {true}
        |}
        |
        |object Main{
        | def test2(): Unit = {
        |    test(new LoopImpl)
        |  }
        |
        |  def test(loop: LoopImpl): Unit = {
        |    loop.finished() match {
        |      case false =>
        |        test(loop)
        |      case true =>
        |
        |    }
        |  }
        |  test2()
        |}
        |
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  "loop3" should "not throw an exception if an instance does not violates its protocol from a method with boolean return values" in {
    val userCode =
      """
        |package compilerPlugin
        |
        |class Typestate(filename: String) extends scala.annotation.StaticAnnotation
        |
        |
        |import scala.util.control.Breaks
        |
        |
        |@Typestate("src\\main\\scala\\exampleProtocols\\Loop.scala") class LoopImpl {
        |  def finished(): Boolean = true
        |}
        |
        |
        |object ClientTest4 extends App{
        |  def test(): Unit = {
        |    val loop = new LoopImpl
        |    val out = new Breaks
        |    val outInner = new Breaks
        |    out.breakable {
        |      do {
        |        outInner.breakable {
        |          loop.finished match {
        |            case false =>
        |              outInner.break()
        |            case true =>
        |              out.break()
        |          }
        |        }
        |      } while ( {
        |        false
        |      })
        |    }
        |  }
        |  test()
        |}
        |""".stripMargin
    noException should be thrownBy {
      val (compiler, sources) = createCompiler(userCode)
      new compiler.Run() compileSources (sources)
    }
  }
  //endregion

  //region <Utility functions>

  def createCompiler(code:String): (Global, List[BatchSourceFile]) ={
    val sources = List(new BatchSourceFile("<test>", code))
    val settings = new Settings
    settings.usejavacp.value = true
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    val compiler = new Global(settings, new ConsoleReporter(settings)) {
      override protected def computeInternalPhases () {
        super.computeInternalPhases
        for (phase <- new GetFileFromAnnotation(this).components)
          phasesSet += phase
      }
    }
    (compiler, sources)
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line.trim())
    }
    bw.close()
  }

  private def deleteFile(path: String) = {
    val fileTemp = new File(path)
    if (fileTemp.exists) {
      fileTemp.delete()
    }
  }

  /** Sorts a set */
  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] = SortedSet.empty[A] ++ unsortedSet
  //endregion
}





