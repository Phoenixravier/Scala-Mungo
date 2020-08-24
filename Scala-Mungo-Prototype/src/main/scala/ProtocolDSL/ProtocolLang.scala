package ProtocolDSL

import java.io.{FileOutputStream, ObjectOutputStream}

import scala.collection.immutable.HashMap
import scala.collection.{SortedSet, mutable}


class ProtocolLang {
  var stateIndexCounter:Int = 1
  var returnValueIndexCounter:Int = 0
  var currentState:State = _
  var currentMethod:Method = _
  var arrayOfStates:Array[Array[State]] = _

  var states: Set[State] = Set()
  var statesMap: HashMap[String, State] = HashMap()

  var methods: Set[Method] = Set()

  case class Transition(startState:State, var method:Method, var returnValue:ReturnValue, nextState:String)
  var transitions: mutable.LinkedHashSet[Transition] = mutable.LinkedHashSet()

  var returnValues: Set[ReturnValue] = Set()

  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] =
    SortedSet.empty[A] ++ unsortedSet

  def in(stateName: String) = new In(stateName)
  class In(val stateName:String) {
    var stateIndex = stateIndexCounter
    if(stateName == "init") {
      stateIndex = 0
      stateIndexCounter -= 1
    }
    currentState = State(stateName, stateIndex)
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
    if(methods.contains(currentMethod)) {
      for(method <- methods){
        if(method.name == currentMethod.name) currentMethod = method
      }
    }
    methods += currentMethod

    def goto(nextState:String) ={
      var returnValue = ReturnValue(currentMethod, null, returnValueIndexCounter)
      for(rv <- returnValues){
        if(rv.parentMethod.name == currentMethod.name) {
          returnValue = rv
          returnValueIndexCounter -=1
        }
      }
      if(currentMethod.indices.isEmpty) currentMethod.indices = Set(returnValueIndexCounter)
      returnValues += returnValue
      transitions += Transition(currentState, currentMethod, returnValue, nextState)
      returnValueIndexCounter +=1
      new At()
    }
  }

  class At(){
    def at(returnValue:String)={
      val lastTransition = transitions.last
      lastTransition.returnValue.valueName = returnValue
      transitions.dropRight(1)
      transitions.add(lastTransition)
      new Or()
    }
  }

  class Or(){
    def or(choiceState:String) ={
      var returnValue = ReturnValue(currentMethod, null, returnValueIndexCounter)
      currentMethod.indices += returnValueIndexCounter
      returnValueIndexCounter +=1
      returnValues += returnValue
      transitions += Transition(currentState, currentMethod, returnValue,choiceState)
      new At()
    }
  }

  def end() = {
    if(!states.exists(_.index == 0)) throw new Exception("No init state found in the protocol, make sure one of your states is called \"init\"")
    val arrayOfStates = createArray()
    printNicely(arrayOfStates)
    sendDataToFile((arrayOfStates, sortSet(states).toArray, returnValues.toArray), "EncodedData.ser")
  }

  def createArray():Array[Array[State]] ={
    arrayOfStates = Array.ofDim[State](states.size, returnValueIndexCounter)
    for (transition <- transitions){
      getStateOrNone(transition.nextState) match{
        case Some(state) => arrayOfStates(transition.startState.index)(transition.returnValue.index) = state
        case None => throw new Exception(s"ERROR, state ${transition.nextState} isn't defined")
      }
    }
    arrayOfStates
  }

  def getStateOrNone(stateName: String): Option[State] ={
    if(statesMap.contains(stateName)) Some(statesMap(stateName))
    else None
  }

  def printNicely(array: Array[Array[State]]): Unit ={
    println()
    sortSet(returnValues).foreach((value: ReturnValue) => print(value+ " "))
    println()
    println("states "+states)
    for(i <- array.indices) {
      print(states.filter(_.index == i).head+" ")
      for(j <- array(i).indices) {
        print(array(i)(j)+ " ")
      }
      println()
    }
  }

  def sendDataToFile(data: (Array[Array[State]], Array[State], Array[ReturnValue]), filename:String): Unit ={
    val oos = new ObjectOutputStream(new FileOutputStream(filename))
    oos.writeObject(data)
    oos.close
  }
}
