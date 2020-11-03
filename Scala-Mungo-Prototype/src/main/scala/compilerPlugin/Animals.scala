package compilerPlugin

class Typestate(filename:String) extends scala.annotation.StaticAnnotation


object Choice1 extends Enumeration {
  type Choice1 = Value
  val OK, ERR = Value
}

object Choice2 extends Enumeration {
  type Choice2 = Value
  val DOT, SUM = Value
}


import javax.net.ssl.SSLSocket
import javax.net.ssl.SSLSocketFactory
import java.io.BufferedReader
import java.io.IOException
import java.io.PrintWriter

import compilerPlugin.CRole.{sslIn, sslOut}
import compilerPlugin.Choice1.OK
import compilerPlugin.Choice2.DOT

import scala.util.control.Breaks

object IntString {
  val sep = " "

  //static string needed for static parser
  // take substring
  // +OK Int String
  // OK Int String
  def Parse(message: String): IntString = {
    val substring = message.substring(4, message.length)
    val pieces = substring.split(sep)
    new IntString(pieces(0).toInt, pieces(1))
  }
}

class IntString(var x: Int, var y: String) {
  override def toString: String = {
    "OK " + this.x + IntString.sep + this.y
  }
}

object SUMIntString {
  val sep = " "

  //static string needed for static parser
  // take substring
  // +OK Int String
  // OK Int String
  def Parse(message: String): SUMIntString = {
    val pieces = message.split(sep)
    new SUMIntString(pieces(0).toInt, pieces(1))
  }
}

class SUMIntString(var x: Int, var y: String) {
  override def toString: String = {
    this.x + SUMIntString.sep + this.y
  }
}


object SUMString {
  def Parse(message: String) = new SUMString(message)
}

class SUMString(var a: String) {
  override def toString: String = {
    this.a
  }
}

object SUMTwoInt {
  val sep = " "

  // take substring
  // use PairInt.java
  // +OK Int Int
  // OK Int Int
  def Parse(message: String): SUMTwoInt = {
    val r = PairInt.Parse(message)
    new SUMTwoInt(r.x, r.y)
  }
}

class SUMTwoInt(var x: Int, var y: Int) {
  override def toString: String = {
    this.x + SUMTwoInt.sep + this.y
  }
}

object PairInt {
  val sep = " "
  def Parse(message: String): PairInt = { //input: "Int Int"
    val pieces = message.split(sep)
    new PairInt(pieces(0).toInt, pieces(1).toInt)
  }
}

class PairInt(var x: Int, var y: Int) {
  override def toString: String = {
    //output: "Int Int"
    this.x + PairInt.sep + this.y
  }
}

object TwoInt {
  val sep = " "
  // take substring
  // use PairInt.java
  // +OK Int Int
  // OK Int Int
  def Parse(message: String): TwoInt = {
    val substring = message.substring(4, message.length)
    val r = PairInt.Parse(substring)
    new TwoInt(r.x, r.y)
  }
}

class TwoInt(var x: Int, var y: Int) {
  override def toString: String = {
    this.x + TwoInt.sep + this.y
  }
}

object ERRString { //static String sep = "-";
  def Parse(message: String): ERRString = {
    var substring = ""
    if (message != null) substring = message.substring(4, message.length)
    new ERRString(substring)
  }
}

class ERRString(var a: String) {
  override def toString: String = {
    val message = "ERR " + this.a
    message
  }
}

object OKString {
  def Parse(message: String): OKString = {
    var substring = ""
    if (message != null) substring = message.substring(3, message.length)
    new OKString(substring)
  }
}

class OKString(var a: String) {
  val label = "OK "

  override def toString: String = {
    val message = this.label + this.a
    message
  }
}

object CRole{
  val sslSocketFactory: SSLSocketFactory = SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
  var sslSocket: SSLSocket = null
  var sslIn: BufferedReader = null
  var sslOut: PrintWriter = null
}

@Typestate("src\\main\\scala\\exampleProtocols\\CProtocol.scala")
class CRole {
  var currentmessage: String = null //to store server messages in
  //reading server responses
  def Servermessage: String = {
    try this.currentmessage = sslIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error.")
        System.exit(-1)
    }
    this.currentmessage
  }

  def receive_OKNStringFromS: OKString = {
    this.Servermessage
    val okn = OKString.Parse(this.currentmessage)
    okn
  }

  def send_USERToS(): Unit = {
    //this.socketSOut.println("USER");
  }

  def send_QUITToS(): Unit = {
    //this.socketSOut.println("QUIT");
  }

  def send_USERStringToS(payload: String): Unit = {
    sslOut.println("USER " + payload)
  }

  def receive_Choice1LabelFromS: Choice1.Value = {
    this.Servermessage
    if (currentmessage != null && this.currentmessage.toString.charAt(0) == '+') Choice1.OK
    else Choice1.ERR
  }

  def receive_OKStringFromS: OKString = {
    val ok = OKString.Parse(this.currentmessage)
    ok
  }

  def send_PASSToS(): Unit = {
    //this.socketSOut.println("PASS");
  }

  def send_PASSStringToS(payload: String): Unit = {
    sslOut.println("PASS " + payload)
  }

  def send_STATToS(): Unit = {
    //this.socketSOut.println("STAT");
  }

  def send_LISTToS(): Unit = {
    //this.socketSOut.println("LIST");
  }

  def send_LIST_NToS(): Unit = {
    //this.socketSOut.println("LIST_N");
  }

  def send_RETR_NToS(): Unit = {
    //this.socketSOut.println("RETR_N");
  }

  def send_DELE_NToS(): Unit = {
    //this.socketSOut.println("DELE_N");
  }

  def send_RSETToS(): Unit = {
    //this.socketSOut.println("RSET");
  }

  def send_TOP_NToS(): Unit = {
    //this.socketSOut.println("TOP_N");
  }

  def send_NOOPToS(): Unit = {
    //this.socketSOut.println("NOOP");
  }

  def send_UIDLToS(): Unit = {
    //this.socketSOut.println("UIDL");
  }

  def send_UIDL_NToS(): Unit = {
    //this.socketSOut.println("UIDL_N");
  }

  def send_STATVoidToS(payload: Void): Unit = {
    sslOut.println("STAT")
  }

  def receive_OKNTwoIntFromS: TwoInt = {
    this.Servermessage
    TwoInt.Parse(this.currentmessage)
  }

  def send_LISTVoidToS(payload: Void): Unit = {
    sslOut.println("LIST")
  }

  def receive_Choice2LabelFromS: Choice2.Value = {
    this.Servermessage
    if (this.currentmessage.toString.charAt(0) == '.') Choice2.DOT
    else Choice2.SUM
  }

  def receive_DOTVoidFromS: Void = null

  def receive_SUMTwoIntFromS: SUMTwoInt = { //sum is always part of choice
    SUMTwoInt.Parse(this.currentmessage)
  }

  def receive_ERRStringFromS: ERRString = { //always part of choice
    ERRString.Parse(this.currentmessage)
  }

  def send_LIST_nIntToS(payload: Int): Unit = {
    sslOut.println("LIST " + payload)
  }

  def receive_OKTwoIntFromS: TwoInt = { //part of choice - do not update servermessage
    TwoInt.Parse(this.currentmessage)
  }

  def send_RETR_nIntToS(payload: Int): Unit = {
    sslOut.println("RETR " + payload)
  }

  def receive_SUMStringFromS: SUMString = { //this.Servermessage(); - part of choice
    SUMString.Parse(this.currentmessage)
  }

  def send_DELE_nIntToS(payload: Int): Unit = {
    sslOut.println("DELE " + payload)
  }

  def send_RSETVoidToS(payload: Void): Unit = {
    sslOut.println("RSET")
  }

  def send_TOP_nTwoIntToS(payload: Nothing): Unit = {
    sslOut.println("TOP " + payload)
  }

  def send_NOOPVoidToS(payload: Void): Unit = {
    sslOut.println("NOOP")
  }

  def receive_OKNVoidFromS: Void = null

  def send_QUITVoidToS(payload: Void): Unit = {
    sslOut.println("QUIT")
  }

  def send_UIDLVoidToS(payload: Void): Unit = {
    sslOut.println("UIDL")
  }

  def receive_SUMIntStringFromS: SUMIntString = { //part of choice - do not call new servermessage
    SUMIntString.Parse(this.currentmessage)
  }

  def send_UIDL_nIntToS(payload: Int): Unit = {
    sslOut.println("UIDL " + payload)
  }

  def receive_OKIntStringFromS: IntString = {
    IntString.Parse(this.currentmessage)
  }
}



import javax.net.ssl.SSLSocket
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.PrintWriter
import java.util.Scanner


object CMain {
  val CRLF = "\\r\\n"

  def safeRead(readerC: BufferedReader): String = {
    var readline = ""
    try readline = readerC.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error, unable to read")
        System.exit(-1)
    }
    readline
  }

  def main(args: Array[String]): Unit = {
    try {
      System.out.println("Starting SSL/POP3 test...\n")
      System.out.println("Creating socket connection.")
      CRole.sslSocket = CRole.sslSocketFactory.createSocket("pop.gmx.co.uk", 995).asInstanceOf[SSLSocket]
      CRole.sslIn = new BufferedReader(new InputStreamReader(CRole.sslSocket.getInputStream))
      CRole.sslOut = new PrintWriter(CRole.sslSocket.getOutputStream, true)
      // Create the current role
      val currentC = new CRole
      // readerC can be used to input strings, and then use them in send method invocation
      val readerC = new BufferedReader(new InputStreamReader(System.in))
      // Method invocation follows the C typestate
      val payload1 = currentC.receive_OKNStringFromS
      System.out.println("Received from S: " + payload1)
      val usernameAuth = new Breaks
      val usernameAuthInner = new Breaks
      val passwordAuth = new Breaks
      val passwordAuthInner = new Breaks
      val transaction = new Breaks
      val transactionInner = new Breaks
      val summaryChoiceUidl = new Breaks
      val summary_choice_top = new Breaks
      usernameAuth.breakable {
        do {
          usernameAuthInner.breakable{
          System.out.print("Choose a label among USER or QUIT: ")
          safeRead(readerC) match {
            case "USER" =>
              currentC.send_USERToS()
              System.out.print("Send username to S: ")
              val payload2 = safeRead(readerC)
              currentC.send_USERStringToS(payload2)
              currentC.receive_Choice1LabelFromS match {
                case OK =>
                  val payload3 = currentC.receive_OKStringFromS
                  System.out.println("Received from S: " + payload3)
                  passwordAuth.breakable {
                    do {
                      passwordAuthInner.breakable {
                        System.out.print("Choose a label among PASS or QUIT: ")
                        safeRead(readerC) match {
                          case "PASS" =>
                            currentC.send_PASSToS()
                            System.out.print("Send password to S: ")
                            val payload4 = safeRead(readerC)
                            currentC.send_PASSStringToS(payload4)
                            currentC.receive_Choice1LabelFromS match {
                              case OK =>
                                val payload5 = currentC.receive_OKStringFromS
                                System.out.println("Received from S: " + payload5)
                                transaction.breakable {
                                  do {
                                    transactionInner.breakable {
                                      System.out.print("Choose a label among [STAT, LIST, LIST_N, RETR_N, DELE_N, RSET, TOP_N, NOOP, QUIT, UIDL, UIDL_N]: ")
                                      safeRead(readerC) match {
                                        case "STAT" =>
                                          currentC.send_STATToS()
                                          val payload6 = null
                                          currentC.send_STATVoidToS(payload6)
                                          val payload7 = currentC.receive_OKNTwoIntFromS
                                          System.out.println("Received from S: OK " + payload7)
                                          transactionInner.break() //todo: continue is not supported

                                        case "LIST" =>
                                          currentC.send_LISTToS()
                                          val payload8 = null
                                          currentC.send_LISTVoidToS(payload8)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload9 = currentC.receive_OKStringFromS
                                              System.out.println("Received from S: " + payload9)
                                              _summary_choice_list //todo: labels are not supported
                                              do currentC.receive_Choice2LabelFromS match {
                                                case DOT =>
                                                  val payload10 = currentC.receive_DOTVoidFromS
                                                  System.out.println("Received from S: .")
                                                  //System.out.println("Received from S: " + payload10);
                                                  transactionInner.break() //todo: continue is not supported

                                                //break summary_choice_list
                                                case Choice2.SUM =>
                                                  val payload11 = currentC.receive_SUMTwoIntFromS
                                                  System.out.println("Received from S: " + payload11)
                                                  continue _summary_choice_list //todo: continue is not supported

                                              } while ( {
                                                true
                                              })
                                            //break _transaction;
                                            case Choice1.ERR =>
                                              val payload12 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload12)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break() //todo: continue is not supported

                                        case "LIST_N" =>
                                          currentC.send_LIST_NToS()
                                          System.out.print("Send messagenumber to S: ")
                                          val keyboard1 = new Scanner(System.in) //read keyboard
                                          val payload13 = keyboard1.nextInt //to declare payload13
                                          currentC.send_LIST_nIntToS(payload13)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload14 = currentC.receive_OKTwoIntFromS
                                              System.out.println("Received from S: OK " + payload14)
                                              transactionInner.break() //todo: continue is not supported

                                            case Choice1.ERR =>
                                              val payload15 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload15)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break() //todo: continue is not supported

                                        case "RETR_N" =>
                                          currentC.send_RETR_NToS()
                                          System.out.print("Send messagenumber to S: ")
                                          val keyboard2 = new Scanner(System.in)
                                          val payload16 = keyboard2.nextInt //to declare payload16
                                          currentC.send_RETR_nIntToS(payload16)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload17 = currentC.receive_OKStringFromS
                                              System.out.println("Received from S: " + payload17)
                                              _summary_choice_retrieve //todo: labels are not supported
                                              do currentC.receive_Choice2LabelFromS match {
                                                case DOT =>
                                                  val payload18 = currentC.receive_DOTVoidFromS
                                                  //System.out.println("Received from S: " + payload18);
                                                  System.out.println("Received from S: .")
                                                  //break _summary_choice_retrieve;
                                                  transactionInner.break() //todo: continue is not supported

                                                case Choice2.SUM =>
                                                  val payload19 = currentC.receive_SUMStringFromS
                                                  //System.out.println("Received from S: " + payload19);
                                                  System.out.println(payload19)
                                                  continue _summary_choice_retrieve //todo: continue is not supported

                                              } while ( {
                                                true
                                              })
                                            case Choice1.ERR =>
                                              val payload20 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload20)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break() //todo: continue is not supported

                                        case "DELE_N" =>
                                          currentC.send_DELE_NToS()
                                          System.out.print("Send messagenumber to S: ")
                                          val keyboard3 = new Scanner(System.in)
                                          val payload21 = keyboard3.nextInt //to declare payload21
                                          currentC.send_DELE_nIntToS(payload21)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload22 = currentC.receive_OKStringFromS
                                              System.out.println("Received from S: " + payload22)
                                              transactionInner.break() //todo: continue is not supported

                                            case Choice1.ERR =>
                                              val payload23 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload23)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break() //todo: continue is not supported

                                        case "RSET" =>
                                          currentC.send_RSETToS()
                                          val payload24 = null
                                          currentC.send_RSETVoidToS(payload24)
                                          val payload25 = currentC.receive_OKNStringFromS
                                          System.out.println("Received from S: " + payload25)
                                          transactionInner.break() //todo: continue is not supported

                                        case "TOP_N" =>
                                          currentC.send_TOP_NToS()
                                          //System.out.print("Send messagenumber and number of lines to S: ");
                                          val keyboard6 = new Scanner(System.in)
                                          System.out.print("Send messagenumber to S: ")
                                          val number1 = keyboard6.nextInt
                                          System.out.print("Send number of lines to S: ")
                                          val number2 = keyboard6.nextInt
                                          val payload26 = new TwoInt(number1, number2)
                                          //String payload26 = safeRead(readerC);
                                          currentC.send_TOP_nTwoIntToS(payload26)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload27 = currentC.receive_OKStringFromS
                                              System.out.println("Received from S: " + payload27)
                                              _summary_choice_top //todo: labels are not supported
                                              do currentC.receive_Choice2LabelFromS match {
                                                case DOT =>
                                                  val payload28 = currentC.receive_DOTVoidFromS
                                                  System.out.println("Received from S: .")
                                                  //System.out.println("Received from S: " + payload28);
                                                  //break _summary_choice_top;
                                                  transactionInner.break() //todo: continue is not supported

                                                case Choice2.SUM =>
                                                  val payload29 = currentC.receive_SUMStringFromS
                                                  System.out.println(/*"Received from S: " + */ payload29)
                                                  continue _summary_choice_top //todo: continue is not supported

                                              } while ( {
                                                true
                                              })
                                            case Choice1.ERR =>
                                              val payload30 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload30)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break() //todo: continue is not supported

                                        case "NOOP" =>
                                          currentC.send_NOOPToS()
                                          val payload31 = null
                                          currentC.send_NOOPVoidToS(payload31)
                                          val payload32 = currentC.receive_OKNVoidFromS
                                          System.out.println("Received from S: " + payload32)

                                          transactionInner.break() //todo: continue is not supported

                                        case "QUIT" =>
                                          currentC.send_QUITToS()
                                          val payload33 = null
                                          currentC.send_QUITVoidToS(payload33)
                                          val payload34 = currentC.receive_OKNStringFromS
                                          System.out.println("Received from S: " + payload34)
                                          transaction.break()

                                        case "UIDL" =>
                                          currentC.send_UIDLToS()
                                          val payload35 = null
                                          currentC.send_UIDLVoidToS(payload35)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload36 = currentC.receive_OKStringFromS
                                              System.out.println("Received from S: " + payload36)
                                              do
                                                summaryChoiceUidl.breakable {
                                                currentC.receive_Choice2LabelFromS match {
                                                  case DOT =>
                                                  val payload37 = currentC.receive_DOTVoidFromS
                                                  System.out.println ("Received from S: .")
                                                    //System.out.println("Received from S: " + payload37);
                                                    //break _summary_choice_uidl;
                                                  transactionInner.break ()
                                                  case Choice2.SUM =>
                                                  val payload38 = currentC.receive_SUMIntStringFromS
                                                  System.out.println ("Received from S: " + payload38)
                                                  summaryChoiceUidl.break()
                                                }
                                              }while ( {
                                                true
                                              })
                                            case Choice1.ERR =>
                                              val payload39 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload39)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break() //todo: continue is not supported

                                        case "UIDL_N" =>
                                          currentC.send_UIDL_NToS()
                                          System.out.print("Send messagenumber to S: ")
                                          val keyboard4 = new Scanner(System.in)
                                          val payload40 = keyboard4.nextInt //to declare payload40
                                          currentC.send_UIDL_nIntToS(payload40)
                                          currentC.receive_Choice1LabelFromS match {
                                            case OK =>
                                              val payload41 = currentC.receive_OKIntStringFromS
                                              System.out.println("Received from S: " + payload41)
                                              transactionInner.break() //todo: continue is not supported

                                            case Choice1.ERR =>
                                              val payload42 = currentC.receive_ERRStringFromS
                                              System.out.println("Received from S: " + payload42)
                                              transactionInner.break() //todo: continue is not supported

                                          }
                                          transactionInner.break()
                                      }
                                    }
                                  } while ( {
                                    true
                                  })
                                }
                                passwordAuth.break()

                              case Choice1.ERR =>
                                val payload43 = currentC.receive_ERRStringFromS
                                System.out.println("Received from S: " + payload43)
                                passwordAuthInner.break()
                            }
                            passwordAuth.break()

                          case "QUIT" =>
                            currentC.send_QUITToS()
                            val payload44 = null
                            currentC.send_QUITVoidToS(payload44)
                            val payload45 = currentC.receive_OKNStringFromS
                            System.out.println("Received from S: " + payload45)
                            passwordAuth.break()
                        }
                      }
                    } while ( {
                      true
                    })
                  }
                  usernameAuth.break()

                case Choice1.ERR =>
                  val payload46 = currentC.receive_ERRStringFromS
                  System.out.println("Received from S: " + payload46)
                  usernameAuthInner.break()
              }
              usernameAuth.break()
            case "QUIT" =>
              currentC.send_QUITToS()
              val payload47 = null
              currentC.send_QUITVoidToS(payload47)
              val payload48 = currentC.receive_OKNStringFromS
              System.out.println("Received from S: " + payload48)
              usernameAuth.break()
          }
        } }while ( {
          true
        })
      }
      CRole.sslIn.close
      CRole.sslOut.close
    } catch {
      case e: IOException =>
        System.out.println("Input/output error")
        System.exit(-1)
    } //end of try

    //end of main}
    //end of class
  }





