
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





