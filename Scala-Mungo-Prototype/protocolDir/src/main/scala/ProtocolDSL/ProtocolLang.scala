package ProtocolDSL


import java.io.{BufferedOutputStream, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.collection.{SortedSet, mutable}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer


class ProtocolLang {
  var stateIndexCounter:Int = 0
  var returnValueIndexCounter:Int = 0
  var currentState:State = _
  var currentMethod:Method = _
  var arrayOfStates:Array[Array[State]] = _

  var states: Set[State] = Set()
  var statesMap: HashMap[String, State] = HashMap()

  var methods: Set[Method] = Set()

  case class Transition(startState:State, var method:Method, var returnValue:ReturnValue, nextState:String)
  var transitions: ArrayBuffer[Transition] = ArrayBuffer()

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
    println(arrayOfStates)
    printNicely(arrayOfStates)
    val meth = Method("walk()", Set(0))
    val methtwo = Method("die()", Set(2,3,4,5))
    val me = Method("die()", Set(2,3,4,5))
    val met = Method("die()", Set(2,3,4,5))
    val m = Method("die()", Set(2,3,4,5))
    val metho = Method("comeAlive()", Set(1))
    val returnValuesArray = returnValues.toArray
    val statesArray:Array[State] = states.toArray
    sendDataToFile((arrayOfStates, statesArray, returnValuesArray), "EncodedData.ser")
    //val stock = getDataFromFile("EncodedData.ser")
  }

  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] =
    SortedSet.empty[A] ++ unsortedSet

  def sendDataToFile(data: (Array[Array[State]], Array[State], Array[ReturnValue]), filename:String): Unit ={
    val oos = new ObjectOutputStream(new FileOutputStream(filename))
    oos.writeObject(data)
    oos.close
  }

  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    println("in getData function")
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close
    println(stock)
    stock
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
