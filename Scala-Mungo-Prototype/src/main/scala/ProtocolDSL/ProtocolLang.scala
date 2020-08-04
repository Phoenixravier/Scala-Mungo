package ProtocolDSL


import java.io.{BufferedOutputStream, BufferedWriter, File, FileOutputStream, FileWriter, IOException}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Paths}


import scala.collection.SortedSet
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import boopickle.Default._
import boopickle.UnpickleImpl

import scala.io.Source.fromFile
import scala.reflect.io.File
//import scala.reflect.io.File

class ProtocolLang {
  var stateIndexCounter:Int = 0
  var returnValueIndexCounter:Int = 0
  var currentState:State = _
  var currentMethod:Method = _
  var arrayOfStates:Array[Array[State]] = _

  case class State(name: String, index: Int){
    override def toString() ={
      name
    }
  }
  var states: Set[State] = Set()
  var statesMap: HashMap[String, State] = HashMap()

  case class Method(name: String, var indices:Set[Int] = Set())
  var methods: Set[Method] = Set()

  case class Transition(startState:State, var method:Method, var returnValue:ReturnValue, nextState:String)
  var transitions: ArrayBuffer[Transition] = ArrayBuffer()

  case class ReturnValue(parentMethod:Method, var valueName:String, index:Int) extends Ordered[ReturnValue]{
    override def compare(that:ReturnValue) ={
      this.index compare that.index
    }
    override def toString()={
      parentMethod.name + ":"+ valueName
    }
  }
  var returnValues: Set[ReturnValue] = Set()

  def in(stateName: String) = new In(stateName)
  class In(val stateName:String) {
    currentState = State(stateName, stateIndexCounter)
    states += currentState
    statesMap += (stateName -> currentState)
    stateIndexCounter += 1
  }
  def when(methodSignature:String) = {
    currentState match{
      case null => throw new Exception("ERROR: when used before in")
      case _ =>
    }
    new Goto(methodSignature)
  }

  class Goto(val methodSignature:String){
    currentMethod = Method(methodSignature)
    methods += currentMethod

    def goto(nextState:String) ={
      var returnValue = ReturnValue(currentMethod, null, returnValueIndexCounter)
      currentMethod.indices += returnValueIndexCounter
      returnValues += returnValue
      transitions += Transition(currentState, currentMethod, returnValue, nextState)
      returnValueIndexCounter +=1
      new At()
    }
  }

  class At(){
    def at(returnValue:String)={
      val lastTransition = transitions(transitions.length-1)
      lastTransition.returnValue.valueName = returnValue
      transitions(transitions.length-1) = lastTransition
      new Or()
    }
  }

  class Or(){
    def or(choiceState:String) ={
      var returnValue = ReturnValue(currentMethod, null, returnValueIndexCounter)
      currentMethod.indices += returnValueIndexCounter
      returnValues += returnValue
      transitions += Transition(currentState, currentMethod, returnValue,choiceState)
      returnValueIndexCounter +=1
      new At()
    }
  }



  def end() = createArray()


  def createArray() ={
    arrayOfStates = Array.ofDim[State](stateIndexCounter, returnValueIndexCounter)
    for (transition <- transitions){
      getStateOrNone(transition.nextState) match{
        case Some(state) => arrayOfStates(transition.startState.index)(transition.returnValue.index) = state
        case None => throw new Exception(s"ERROR, state ${transition.nextState} isn't defined")
      }
    }
    printNicely(arrayOfStates)
    sendDataToFile(arrayOfStates, "EncodedArray.txt")
    getArrayFromFile("EncodedArray.txt")
  }


  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] =
    SortedSet.empty[A] ++ unsortedSet


  def sendDataToFile(data: Array[Array[State]], filename:String): Unit ={
    val pickledArrayByteBuffer = Pickle.intoBytes(data)
    //need to convert into an array of bytes to be able to print into a file and get back nicely
    val pickledByteArray = Array.ofDim[Byte](pickledArrayByteBuffer.remaining())
    pickledArrayByteBuffer.get(pickledByteArray)

    writeArrayOfBytesToFile(filename, pickledByteArray)
  }

  def writeArrayOfBytesToFile(filename: String, bytes: Array[Byte]): Unit = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(bytes)
    bos.close()
  }

  def getArrayFromFile(filename: String): Unit ={
    val byteArray = Files.readAllBytes(Paths.get(filename))
    //need to convert array of bytes back into a bytebuffer
    val unpickled = UnpickleImpl[Array[Array[State]]].fromBytes(ByteBuffer.wrap(byteArray))
    printNicely(unpickled)
  }

  def printNicely(array: Array[Array[State]]): Unit ={
    println()
    sortSet(returnValues).foreach((value: ReturnValue) => print(value + " "))
    println()
    for(i <- array.indices) {
      print(states.filter(_.index == i).head +" ")
      for(j <- array(i).indices) {
        print(array(i)(j)+ " ")
      }
      println()
    }
  }

  def getStateOrNone(stateName: String): Option[State] ={
    if(statesMap.contains(stateName)) Some(statesMap(stateName))
    else None
  }

}
