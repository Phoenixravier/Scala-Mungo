
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





