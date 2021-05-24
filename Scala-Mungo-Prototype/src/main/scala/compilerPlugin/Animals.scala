package compilerPlugin

//class Typestate(filename:String) extends scala.annotation.StaticAnnotation


import javax.net.ssl.SSLSocket
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.PrintWriter
import java.net.Socket
import java.net.UnknownHostException

import scala.util.control.Breaks


object Choice1 extends Enumeration {
  type Choice1 = Value
  val _250DASH, _250 = Value
}

object Choice2 extends Enumeration {
  type Choice2 = Value
  val _235, _535 = Value
}

object Choice3 extends Enumeration {
  type Choice3 = Value
  val _501, _250 = Value
}

object Choice4 extends Enumeration {
  type Choice4 = Value
  val _250 = Value
}

object SMTPMessage {
  def Parse(message: String): SMTPMessage = {
    val matches = message.split(" |-", 2)
    new SMTPMessage(matches(0), matches(1), message.charAt(3) == '-')
  }
}

class SMTPMessage {
  private var command:String = null
  private var payload:String = null
  private var isDashed = false

  def this(command: String, payload: String, isDashed: Boolean) {
    this()
    this.command = command
    this.payload = payload
    this.isDashed = isDashed
  }

  def this(command: String, payload: String) {
    this()
    this.command = command
    this.payload = payload
    this.isDashed = false
  }

  def this(command: String) {
    this()
    this.command = command
    this.payload = null
    this.isDashed = false
  }

  override def toString: String = {
    var message:String = null
    if (this.payload == null) message = this.command + "\\r\\n"
    else message = this.command + " " + this.payload + "\\r\\n"
    message
  }

  def getCommand: String = this.command

  def getPayload: String = this.payload

  def getIsDashed: Boolean = this.isDashed
}

@Typestate("SMTPCProtocol") class CRole {
  var socketSIn: BufferedReader = null
  var socketSOut: PrintWriter = null
  var socketS: Socket = null
  var sslSocket: SSLSocket = null
  try { //socketS = new Socket("smtp.gmail.com", 25);
    socketS = new Socket("smtp.gmail.com", 587)
    socketSIn = new BufferedReader(new InputStreamReader(socketS.getInputStream))
    socketSOut = new PrintWriter(socketS.getOutputStream, true)
  } catch {
    case e: UnknownHostException =>
      System.out.println("Unable to connect to the remote host")
      System.exit(-(1))
    case e: IOException =>
      System.out.println("Input/output error")
      System.exit(-(1))
  }

  // Typestate method definitions
  def receive_220StringFromS(): String = {
    var line = ""
    try line = this.socketSIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Outpur error.")
        System.exit(-(1))
    }
    return line
  }

  def send_EHLOToS(): Unit = {
    // This method corresponds to selecting the command EHLO,
    // hence its body is empty.
  }

  def send_QUITToS(): Unit = {
    // This method corresponds to selecting the command QUIT,
  }

  def send_ehloStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def receive_CChoice1LabelFromS(): Choice1.Value = {
    var stringLabelChoice1 = ""
    try {
      stringLabelChoice1 = this.socketSIn.readLine
      System.out.println(stringLabelChoice1)
    } catch {
      case e: IOException =>
        System.out.println("Input/Outpur error, unable to get label")
        System.exit(-(1))
    }
    val message = SMTPMessage.Parse(stringLabelChoice1)
    val intLabelChoice1 = message.getCommand.toInt
    intLabelChoice1 match {
      case 250 =>
        if (message.getIsDashed) {
          Choice1._250DASH
        }
        else {
          Choice1._250
        }
      case _ =>
        Choice1._250
    }
  }

  def receive_250dashStringFromS(): String = {
    return ""
  }

  def receive_250StringFromS(): String = {
    return ""
  }

  def send_STARTTLSToS(): Unit = {
    // This method corresponds to selecting the command STARTTLS,
  }

  def send_starttlsStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def send_AUTHToS(): Unit = {
    // This method corresponds to selecting the command AUTH,
  }

  def send_authStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def receive_CChoice2LabelFromS(): Choice2.Value = {
    var stringLabelChoice2 = ""
    try stringLabelChoice2 = this.socketSIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Outpur error, unable to get label")
        System.exit(-(1))
    }
    val message = SMTPMessage.Parse(stringLabelChoice2)
    val intLabelChoice2 = message.getCommand.toInt
    intLabelChoice2 match {
      case 235 =>
        Choice2._235
      case 535 =>
        Choice2._535
      case _ =>
        Choice2._535
    }
  }

  def receive_235StringFromS(): String = {
    return ""
  }

  def send_MAILToS(): Unit = {
    // This method corresponds to selecting the command MAIL,
  }

  def send_mailStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def receive_CChoice3LabelFromS(): Choice3.Value = {
    var stringLabelChoice3 = ""
    try stringLabelChoice3 = this.socketSIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Outpur error, unable to get label")
        System.exit(-(1))
    }
    val message = SMTPMessage.Parse(stringLabelChoice3)
    val intLabelChoice3 = message.getCommand.toInt
    intLabelChoice3 match {
      case 501 =>
        Choice3._501
      case 250 =>
        Choice3._250
      case _ =>
        Choice3._250
    }
  }

  def receive_501StringFromS(): String = {
    return ""
  }

  def send_RCPTToS(): Unit = {
    // This method corresponds to selecting the command RCPT,
  }

  def send_DATAToS(): Unit = {
    // This method corresponds to selecting the command DATA,
  }

  def send_rcptStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def receive_CChoice4LabelFromS(): Choice4.Value = {
    var stringLabelChoice4 = ""
    try stringLabelChoice4 = this.socketSIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Outpur error, unable to get label")
        System.exit(-(1))
    }
    val message = SMTPMessage.Parse(stringLabelChoice4)
    val intLabelChoice4 = message.getCommand.toInt
    intLabelChoice4 match {
      case 250 =>
        Choice4._250
      case _ =>
        Choice4._250
    }
  }

  def send_dataStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def receive_354StringFromS(): String = {
    var line = ""
    try line = this.socketSIn.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Outpur error.")
        System.exit(-(1))
    }
    return line
  }

  def send_DATALINEToS(): Unit = {
    // This method corresponds to selecting the command DATALINE,
  }

  def send_SUBJECTToS(): Unit = {
    // This method corresponds to selecting the command SUBJECT,
  }

  def send_ATADToS(): Unit = {
    // This method corresponds to selecting the command ATAD,
  }

  def send_datalineStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def send_subjectStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def send_atadStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def send_quitStringToS(payload: String): Unit = {
    this.socketSOut.print(payload)
    this.socketSOut.flush()
  }

  def receive_535StringFromS(): String = {
    return ""
  }
}

import sun.misc.BASE64Encoder
import javax.net.ssl.SSLSocket
import javax.net.ssl.SSLSocketFactory
import java.io._
import java.net.UnknownHostException


object CMain {
  val CRLF: String = "\\r\\n"

  def safeRead(readerC: BufferedReader): String = {
    var readline: String = ""
    try readline = readerC.readLine
    catch {
      case e: IOException =>
        System.out.println("Input/Output error, unable to read")
        System.exit(-(1))
    }
    return readline
  }

  def main(): Unit = {
    val X = new Breaks
    val XInner = new Breaks
    val X1 = new Breaks
    val X1Inner = new Breaks
    val Y = new Breaks
    val YInner = new Breaks
    val Z1 = new Breaks
    val Z1Inner = new Breaks
    val Z2 = new Breaks
    val Z2Inner = new Breaks
    val Z3 = new Breaks
    val Z3Inner = new Breaks

    val currentC = new CRole
    // readerC can be used to input strings, and then use them in send method invocation
    val readerC: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    // Method invocation follows the C typestate
    currentC.receive_220StringFromS() //S1
    safeRead(readerC) match {
      case "EHLO" =>
        currentC.send_EHLOToS() //S2
        currentC.send_ehloStringToS((new SMTPMessage("EHLO", safeRead(readerC))).toString) //S3
        X.breakable {
          do {
            XInner.breakable {
              currentC.receive_CChoice1LabelFromS() match {
                case Choice1._250DASH => // S4
                  currentC.receive_250dashStringFromS() //S3
                  XInner.break() //--
                //S3
                case Choice1._250 => // S5
                  currentC.receive_250StringFromS() //S6
                  X.break() //--
              } //--
            } //S3
          } while (true) //--
        } //S6
    }
  }

  def main(args: Array[String]): Unit = { // Create the current role
    val X = new Breaks
    val XInner = new Breaks
    val X1 = new Breaks
    val X1Inner = new Breaks
    val Y = new Breaks
    val YInner = new Breaks
    val Z1 = new Breaks
    val Z1Inner = new Breaks
    val Z2 = new Breaks
    val Z2Inner = new Breaks
    val Z3 = new Breaks
    val Z3Inner = new Breaks

    val currentC = new CRole
    // readerC can be used to input strings, and then use them in send method invocation
    val readerC: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    // Method invocation follows the C typestate
    val rawMessage = currentC.receive_220StringFromS()
    val payload1: SMTPMessage = SMTPMessage.Parse(rawMessage)
    System.out.println("Received from S: " + payload1)
    System.out.print("Choose a label among EHLO or QUIT: ")
    safeRead(readerC) match {
      case "EHLO" =>
        currentC.send_EHLOToS()
        System.out.print("Send to S text for EHLO: ")
        val payload2: String = safeRead(readerC)
        currentC.send_ehloStringToS((new SMTPMessage("EHLO", payload2)).toString)
        X.breakable {
          do {
            XInner.breakable {
              {
                currentC.receive_CChoice1LabelFromS() match {
                  case Choice1._250DASH =>
                    val payload3: String = currentC.receive_250dashStringFromS()
                    XInner.break()
                  case Choice1._250 =>
                    val payload4: String = currentC.receive_250StringFromS()
                    System.out.print("Choose a label among STARTTLS or QUIT: ")
                    safeRead(readerC) match {
                      case "STARTTLS" =>
                        currentC.send_STARTTLSToS()
                        currentC.send_starttlsStringToS((new SMTPMessage("STARTTLS")).toString)
                        val rawMessage = currentC.receive_220StringFromS
                        val payload6: SMTPMessage = SMTPMessage.Parse(rawMessage)
                        try {
                          currentC.sslSocket = (SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]).createSocket(currentC.socketS, currentC.socketS.getInetAddress.getHostAddress, currentC.socketS.getPort, true).asInstanceOf[SSLSocket]
                          currentC.socketSIn = new BufferedReader(new InputStreamReader(currentC.sslSocket.getInputStream))
                          currentC.socketSOut = new PrintWriter(currentC.sslSocket.getOutputStream, true)
                        } catch {
                          case e: UnknownHostException =>
                            System.out.println("Unable to connect to the remote host")
                            System.exit(-(1))
                          case e: IOException =>
                            System.out.println("Input/output error")
                            System.exit(-(1))
                        }
                        System.out.println("Received from S: " + payload6)
                        System.out.print("Choose a label among EHLO, or QUIT: ")
                        val label3: Int = if (safeRead(readerC) == "EHLO") {
                          1
                        }
                        else {
                          2
                        }
                        label3 match {
                          case 1 =>
                            currentC.send_EHLOToS()
                            System.out.print("Send to S text for EHLO: ")
                            val payload7: String = safeRead(readerC)
                            currentC.send_ehloStringToS((new SMTPMessage("EHLO", payload7)).toString)
                            X1.breakable {
                              do {
                                X1Inner.breakable {
                                  {
                                    currentC.receive_CChoice1LabelFromS match {
                                      case Choice1._250DASH =>
                                        val payload8: String = currentC.receive_250dashStringFromS
                                        X1Inner.break()
                                      case Choice1._250 =>
                                        val payload9: String = currentC.receive_250StringFromS
                                        Y.breakable {
                                          do {
                                            YInner.breakable {
                                              {
                                                System.out.print("Choose a label among AUTH or QUIT: ")
                                                val label4: Int = if (safeRead(readerC) == "AUTH") {
                                                  1
                                                }
                                                else {
                                                  2
                                                }
                                                label4 match {
                                                  case 1 =>
                                                    currentC.send_AUTHToS()
                                                    System.out.print("Username: ")
                                                    val username: String = safeRead(readerC)
                                                    val console: Console = System.console
                                                    val tmp: Array[AnyRef] = Array()
                                                    val password: String = new String(console.readPassword("Password: ", tmp))
                                                    var token: String = ""
                                                    try {
                                                      val encoder: BASE64Encoder = new BASE64Encoder
                                                      token = encoder.encodeBuffer((username + "\u0000" + username + "\u0000" + password).getBytes("UTF-8")).trim
                                                    } catch {
                                                      case e: IOException =>
                                                        System.out.println("unable to use base64 encoding")
                                                    }
                                                    currentC.send_authStringToS((new SMTPMessage("AUTH PLAIN", token)).toString)
                                                    currentC.receive_CChoice2LabelFromS match {
                                                      case Choice2._235 =>
                                                        val payload11: String = currentC.receive_235StringFromS
                                                        Z1.breakable {
                                                          do {
                                                            Z1Inner.breakable {
                                                              {
                                                                System.out.print("Choose a label among MAIL or QUIT: ")
                                                                val label5: Int = if (safeRead(readerC) == "MAIL") 1 else 2
                                                                label5 match {
                                                                  case 1 =>
                                                                    currentC.send_MAILToS()
                                                                    System.out.print("Email from: ")
                                                                    val payload12: String = safeRead(readerC)
                                                                    currentC.send_mailStringToS((new SMTPMessage("MAIL FROM:<" + payload12 + ">")).toString)
                                                                    currentC.receive_CChoice3LabelFromS match {
                                                                      case Choice3._501 =>
                                                                        val payload13: String = currentC.receive_501StringFromS
                                                                        Z1Inner.break()
                                                                      case Choice3._250 =>
                                                                        val payload14: String = currentC.receive_250StringFromS
                                                                        System.out.println("Received from S: " + payload14)
                                                                        Z2.breakable {
                                                                          do {
                                                                            Z2Inner.breakable {
                                                                              {
                                                                                System.out.print("Choose a label among RCPT or DATA: ")
                                                                                val label6: Int = if (safeRead(readerC) == "RCPT") {
                                                                                  1
                                                                                }
                                                                                else {
                                                                                  2
                                                                                }
                                                                                label6 match {
                                                                                  case 1 =>
                                                                                    currentC.send_RCPTToS()
                                                                                    System.out.print("Send to S text for RCPT: ")
                                                                                    val payload15: String = safeRead(readerC)
                                                                                    currentC.send_rcptStringToS((new SMTPMessage("RCPT TO:<" + payload15 + ">")).toString)
                                                                                    currentC.receive_CChoice4LabelFromS() match {
                                                                                      case Choice4._250 =>
                                                                                        val payload16: String = currentC.receive_250StringFromS()
                                                                                        Z2Inner.break()
                                                                                    }
                                                                                    Z2.break()
                                                                                  case 2 =>
                                                                                    currentC.send_DATAToS()
                                                                                    currentC.send_dataStringToS((new SMTPMessage("DATA")).toString)
                                                                                    val payload18: String = currentC.receive_354StringFromS
                                                                                    System.out.println("Received from S: " + payload18)
                                                                                    do {
                                                                                      Z3Inner.breakable{
                                                                                        System.out.print("Choose a label among DATALINE, SUBJECT or ATAD: ")
                                                                                        safeRead(readerC) match {
                                                                                          case "DATALINE" =>
                                                                                            currentC.send_DATALINEToS()
                                                                                            System.out.print("Send to S text for DATALINE: ")
                                                                                            val payload19: String = safeRead(readerC)
                                                                                            currentC.send_datalineStringToS(payload19 + CRLF)
                                                                                            Z3Inner.break()
                                                                                          case "SUBJECT" =>
                                                                                            currentC.send_SUBJECTToS()
                                                                                            System.out.print("Send to S text for SUBJECT: ")
                                                                                            val payload20: String = safeRead(readerC)
                                                                                            currentC.send_subjectStringToS((new SMTPMessage("SUBJECT:" + payload20, CRLF)).toString)
                                                                                            Z3Inner.break()
                                                                                          case "ATAD" =>
                                                                                            currentC.send_ATADToS()
                                                                                            currentC.send_atadStringToS("." + CRLF)
                                                                                            val payload22: String = currentC.receive_250StringFromS
                                                                                            System.out.println("Received from S: " + payload22)
                                                                                            Z1Inner.break()
                                                                                        }
                                                                                      }
                                                                                    } while (true)
                                                                                }
                                                                              }
                                                                            }
                                                                          } while (true)
                                                                        }
                                                                        Z1.break()
                                                                    }
                                                                    Z1.break()
                                                                  case 2 =>
                                                                    currentC.send_QUITToS()
                                                                    val payload23: String = ""
                                                                    currentC.send_quitStringToS(payload23)
                                                                    Z1.break()
                                                                }
                                                              }
                                                            }
                                                          } while (true)
                                                        }
                                                        Y.break()
                                                      case Choice2._535 =>
                                                        val payload24: String = currentC.receive_535StringFromS
                                                        //System.out.println("Received from S: error " + payload24);
                                                        YInner.break()
                                                    }
                                                    Y.break()
                                                  case 2 =>
                                                    currentC.send_QUITToS()
                                                    val payload25: String = ""
                                                    currentC.send_quitStringToS(payload25)
                                                    Y.break()
                                                }
                                              }
                                            }
                                          } while (true)
                                        }
                                        X1.break()
                                    }
                                  }
                                }
                              } while (true)
                            }
                            X.break()
                          case 2 =>
                            currentC.send_QUITToS()
                            val payload26: String = ""
                            currentC.send_quitStringToS(payload26)
                            X.break()
                        }
                        X.break()
                      case "QUIT" =>
                        currentC.send_QUITToS()
                        val payload27: String = ""
                        currentC.send_quitStringToS(payload27)
                        X.break()
                    }
                    X.break()
                }
              }
            }
          } while (true)
        }
      case "QUIT" =>
        currentC.send_QUITToS()
        val payload28: String = ""
        currentC.send_quitStringToS(payload28)
    }
    currentC.send_quitStringToS("quit")
  }
}

