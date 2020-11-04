package compilerPlugin

class Typestate(filename: String) extends scala.annotation.StaticAnnotation


import compilerPlugin.Status.OK

import scala.util.control.Breaks


object Status extends Enumeration {
  type Status = Value
  val OK, ERROR = Value
}


object File {
  def main(args: Array[String]): Unit = {
    val myFile = new File("file.txt")
    val a = myFile
    processFile(a)
  }

  def processFile(myFile: File): Unit = {
    myFile.open match {
      case OK =>
        if (true) {
          val loop = new Breaks
          loop.breakable {
            while (true) {
              myFile.hasNext match {
                case true =>
                  myFile.read()

                case false =>
                  loop.break()
              }
            }
          }
        }
        myFile.close()

      case Status.ERROR =>
        System.out.println("File <file.txt> not found!")

    }
  }
}

@Typestate("src\\main\\scala\\exampleProtocols\\FileProtocol.scala")
class File(var file: String) {
  protected var reader: MyBufferedReader = null
  private var readBuffer: Array[Char] = null
  private var i = 0
  reader = new MyBufferedReader(file)
  readBuffer = new Array[Char](1024)
  i = 0


  def open(): Status.Value = {
    if (reader.open) return Status.OK
    Status.ERROR
  }

  def close(): Unit = {
    reader.close()
  }

  //The next two methods demonstrate that
  // a created typestate object can
  // be assigned in a linear way and
  // passed around as an argument
  def hasNext(): Boolean = {
    if (reader.ready) return true
    false
  }

  def read(): Unit = {
    readBuffer({
      i += 1;
      i - 1
    }) = reader.read
  }
}

import java.io.BufferedReader
import java.io.FileNotFoundException
import java.io.FileReader
import java.io.IOException

class MyBufferedReader(var file: String) {
  private var reader: BufferedReader = null

  def open: Boolean = {
    try reader = new BufferedReader(new FileReader(file))
    catch {
      case e: FileNotFoundException =>
        return false
    }
    true
  }

  def close(): Unit = {
    try reader.close()
    catch {
      case e: IOException =>
        e.printStackTrace()
        System.exit(-1)
    }
  }

  def ready: Boolean = {
    try if (reader.ready) return true
    catch {
      case e: IOException =>
        return false
    }
    false
  }

  def read: Char = {
    var c = -1
    try c = reader.read
    catch {
      case e: IOException =>
        e.printStackTrace()
        System.exit(-1)
    }
    c.toChar
  }
}








