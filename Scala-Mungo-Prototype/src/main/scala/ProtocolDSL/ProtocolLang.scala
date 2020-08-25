package ProtocolDSL

import java.io.{FileOutputStream, ObjectOutputStream}
import scala.collection.immutable.HashMap
import scala.collection.{SortedSet, mutable}

class ProtocolLang {
  var stateIndexCounter:Int = 1 //start indexing at one because init state will take index 0
  var returnValueIndexCounter:Int = 0

  var currentState:State = _
  var currentMethod:Method = _
  var arrayOfStates:Array[Array[State]] = _

  var states: Set[State] = Set()
  var statesMap: HashMap[String, State] = HashMap()
  var methods: Set[Method] = Set()
  var transitions: mutable.LinkedHashSet[Transition] = mutable.LinkedHashSet()
  var returnValues: Set[ReturnValue] = Set()

  val Undefined = "_Undefined_"
  val Any = "_Any_"

  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] =
    SortedSet.empty[A] ++ unsortedSet

  def in(stateName: String) = new In(stateName)
  class In(val stateName:String) {
    if(stateName == null) throw new Exception("You cannot call your state null")
    if(stateName == Undefined) throw new Exception(s"You cannot call a state $Undefined")
    var stateIndex = stateIndexCounter
    if(stateName == "init") {
      stateIndex = 0
      stateIndexCounter -= 1
    }
    currentState = State(stateName, stateIndex)
    if(states.exists(_.name == currentState.name))
      throw new Exception(s"State $stateName defined multiple times, define a state only once")
    states += currentState
    statesMap += (stateName -> currentState)
    stateIndexCounter += 1
  }
  def when(methodSignature:String) = {
    if(methodSignature == null)
      throw new Exception(s"You cannot call your method null. You called a method null in state $currentState")
    currentState match{
      case null =>
        throw new Exception("Defined a method without being inside a state. " +
          "Use in(State) to define a state above a when(method) statement")
      case _ =>
    }
    new Goto(methodSignature)
  }

  class Goto(val methodSignature:String){
    currentMethod = Method(methodSignature, currentState)
    if(methods.contains(currentMethod)) {
      for(method <- methods){
        if(method.currentState == currentState)
          throw new Exception(s"Defined method $methodSignature for state $currentState more than once")
        if(method.name == currentMethod.name) {
          currentMethod = method
          currentMethod.currentState = currentState
        }
      }
    }
    methods += currentMethod

    def goto(nextState:String) ={
      var returnValue = ReturnValue(currentMethod, Any, returnValueIndexCounter)
      for(rv <- returnValues){
        if(rv.parentMethod.name == currentMethod.name && rv.valueName == Any) {
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
      if(returnValue == Any || returnValue == Undefined)
        throw new Exception(s"You used $returnValue in state $currentState as a return value for method ${currentMethod.name}." +
          s"It is not allowed to use $Any or $Undefined as return values for a method")
      //checks if a return value is defined multiple times for the same state and method and throws an error
      for(transition <- transitions){
        if(transition.startState == currentState &&
          transition.method == currentMethod &&
          transition.returnValue.valueName == returnValue)
            throw new Exception(
              s"Defined return value $returnValue for method ${currentMethod.name} " +
                s"in state ${currentState.name} more than once")
      }
      //corrects return value in transition just defined
      val lastTransition = transitions.last
      if(lastTransition.returnValue.valueName == Any) {
        var newReturnValue = ReturnValue(currentMethod, returnValue, returnValueIndexCounter)
        for(rv <- returnValues){
          if(rv.parentMethod.name == currentMethod.name && rv.valueName == returnValue) {
            newReturnValue = rv
            returnValueIndexCounter -=1
          }
        }
        lastTransition.returnValue = newReturnValue
        returnValues += newReturnValue
        returnValueIndexCounter+=1
      }
      else lastTransition.returnValue.valueName = returnValue
      transitions.dropRight(1)
      transitions.add(lastTransition)
      new Or()
    }
  }

  class Or(){
    def or(choiceState:String) ={
      var returnValue = ReturnValue(currentMethod, Undefined, returnValueIndexCounter)
      transitions += Transition(currentState, currentMethod, returnValue,choiceState)
      //Updates
      currentMethod.indices += returnValueIndexCounter
      returnValueIndexCounter +=1
      returnValues += returnValue
      new At()
    }
  }

  def end() = {
    if(returnValues.exists(_.valueName == Undefined)) {
      val problematicTransition = transitions.filter(_.returnValue.valueName == Undefined).head
      throw new Exception(
        s"Defined state ${problematicTransition.nextState} to move to " +
          s"in state ${problematicTransition.startState} without defining a return value. " +
          s"Add an at after the or keyword")
    }
    if(!states.exists(_.index == 0))
      throw new Exception("No init state found in the protocol, make sure one of your states is called \"init\"")
    val arrayOfStates = createArray()
    printNicely(arrayOfStates)
    sendDataToFile((arrayOfStates, sortSet(states).toArray, returnValues.toArray), "EncodedData.ser")
  }

  def createArray():Array[Array[State]] ={
    arrayOfStates = Array.fill(states.size, returnValues.size)(State(Undefined, -1))
    //arrayOfStates = Array.ofDim[State](states.size, returnValues.size)
    for (transition <- transitions){
      if(statesMap.contains(transition.nextState))
        arrayOfStates(transition.startState.index)(transition.returnValue.index) = statesMap(transition.nextState)
      else throw new Exception(s"State ${transition.nextState}, " +
        s"used in state ${transition.startState} with method ${transition.method.name}, isn't defined")
    }
    arrayOfStates
  }


  def printNicely(array: Array[Array[State]]): Unit ={
    println()
    sortSet(returnValues).foreach((value: ReturnValue) => print(value+ " "))
    println()
    println("----------------------------------------------------------------------------------------------------------")
    for(i <- array.indices) {
      print(states.filter(_.index == i).head+" : ")
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
