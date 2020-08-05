import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}


@SerialVersionUID(123L)
case class TestState(name: String, index: Int) extends Serializable {

  override def toString() ={
    name
  }

  def sendDataToFile(data: Array[Array[TestState]], filename: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(filename))
    oos.writeObject(data)
    oos.close
  }

  def getDataFromFile(filename: String): Unit = {
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[Array[Array[TestState]]]
    println(stock)
    ois.close
  }
}

object main3 extends App {
  val state = TestState("state", 1)
  val arrayOfstate = Array.ofDim[TestState](3)
  val arrayOfArray = Array.ofDim[Array[TestState]](10)
  arrayOfstate(0) = state
  arrayOfstate(1) = null
  arrayOfArray(0) = arrayOfstate
  arrayOfArray(1) = Array.ofDim[TestState](3)
  println("hello")
  state.sendDataToFile(arrayOfArray, "file.ser")
  state.getDataFromFile("file.ser")
}

