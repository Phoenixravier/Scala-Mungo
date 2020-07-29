package ProtocolDSL

import scala.collection.immutable.HashMap
import scala.meta.Term.If

class ProtocolLang {
  var stateIndexCounter:Int = 0
  var methodIndexCounter:Int = 0
  var currentState:State = _
  var arrayOfStates:Array[Array[State]] = _

  case class State(val name: String, val index: Int)
  var states: Set[State] = Set()
  var statesMap: HashMap[String, State] = HashMap()

  case class Method(val name: String, val index: Int)
  var methods: Set[Method] = Set()
  var methodsMap: HashMap[String, Int] = HashMap()

  case class Transition(val startState:State, val method:Method, val nextState:String)
  var transitions: Set[Transition] = Set()

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
    val method = Method(methodSignature, methodIndexCounter)
    methods += method
    methodsMap += (methodSignature -> methodIndexCounter)
    methodIndexCounter+=1
    def goto(nextState:String) ={
      transitions += Transition(currentState, method, nextState)
      new At()
    }
  }

  class Or(){
    def or(choiceState:String) ={
      println(choiceState)
      new At()
    }
  }

  class At(){
    def at(choiceLabel:String)={
      println(choiceLabel)
      new Or()
    }
  }

  def end() = createArray()


  def createArray() ={
    arrayOfStates = Array.ofDim[State](stateIndexCounter, methodIndexCounter)
    for (transition <- transitions){
      getStateOrNone(transition.nextState) match{
        case Some(state) => arrayOfStates(transition.startState.index)(transition.method.index) = state
        case None => throw new Exception(s"ERROR, state ${transition.nextState} isn't defined")
      }
    }
    for(row <- arrayOfStates) for(state <- row) println(state)
  }


  def getStateOrNone(stateName: String): Option[State] ={
    if(statesMap.contains(stateName)) Some(statesMap(stateName))
    else None
  }

}
