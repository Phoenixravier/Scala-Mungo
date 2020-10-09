package ProtocolDSL

import java.io.{FileOutputStream, ObjectOutputStream}
import scala.collection.immutable.HashMap
import scala.collection.{SortedSet, mutable}

/** Domain specific language for users to write in their protocols.
 *  Extend this to write a protocol for Scala-Mungo
 *  Use in("Statex") to define a state and then define transitions in it with
 *  when("methodName(argumentTypes)") goto "Statex"
 *  A transition dependent on the return value of a function can be defined by using the "at" and "or" keywords as so:
 *  when("methodName(argumentTypes)") goto
 *    "Statex" at "returnValue1" or
 *    "Statey" at "returnValue2" or
 *    "Statez" at "returnValue3"
 *
 *  One of the states must be "init" and elements will go to this state when initialised in the code.
 *  Always add "end()" to the end of the protocol or it will not work
 *
 *  Example:
 *  object CatProtocol extends ProtocolLang with App{
 *  in ("init")
 *  when ("walk(Int):Unit") goto "State1"
 *  when("comeAlive():Boolean") goto
 *    "init" at "true" or
 *    "State1" at "false"
 *  in ("State1")
 *  in ("State2")
 *  when("walk(Int):Unit") goto "init"
 *  end()
 *  }
 *
 *
 */
class ProtocolLang {
  var stateIndexCounter:Int = 1 //start indexing at one because init state will take index 0
  var returnValueIndexCounter:Int = 0

  var currentState:State = _
  var currentMethod:Method = _

  var states: Set[State] = Set()
  var statesMap: HashMap[String, State] = HashMap()
  var methods: Set[Method] = Set()
  var transitions: mutable.LinkedHashSet[Transition] = mutable.LinkedHashSet()
  var returnValues: Set[ReturnValue] = Set()

  val Undefined = "_Undefined_"
  val Any = "_Any_"

  var ended = false

  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] = SortedSet.empty[A] ++ unsortedSet

  /** Creates a new state with name stateName. The name cannot be null or "_Undefined_".
   *  All state names defined within a single protocol must be unique.
   *
   *  Checks the protocol has not already been ended before this statement
   *  and that the name given is neither null nor "_Undefined_".
   *  It then creates a State object and sets it as the currentState.
   *  Then it checks that this state has not been defined before.
   *  Then it adds the state to the set of states and to the map of stateName to State
   *
   * @param stateName Name of the state defined
   * @return
   */
  def in(stateName: String) = new In(stateName)
  class In(val stateName:String) {
    checkProtocolHasNotBeenEnded()
    checkStateNameIsValid(stateName)
    currentState = createNewState(stateName)
    checkForDuplicateState(currentState)
    //Update datastructures
    states += currentState
    statesMap += (stateName -> currentState)
  }

  /** Checks if the end function has been executed already. Throws an exception if it has.*/
  def checkProtocolHasNotBeenEnded(): Unit ={
    if(ended) throw new Exception("You wrote protocol code after writing end, write all the protocol code before end")
  }

  /** Checks the stateName is neither null nor "_Undefined_". Throws exceptions if the stateName is one of those*/
  def checkStateNameIsValid(stateName:String): Unit ={
    if(stateName == null) throw new Exception("You cannot call your state null")
    if(stateName == Undefined) throw new Exception(s"You cannot call a state $Undefined")
  }

  /** Creates a new state object with a unique integer id. If the stateName is "init", creates a state with id 0. */
  def createNewState(stateName:String):State={
    var newState = State(stateName, stateIndexCounter)
    stateIndexCounter += 1
    if(stateName == "init") {
      newState = State(stateName, 0) //init always gets 0 as index
      stateIndexCounter -= 1 //haven't used the counter so decrement it back
    }
    newState
  }

  /** Checks if the state given has a name which is already another state's one.
   * Throws an exception if there is such a duplicate.*/
  def checkForDuplicateState(state:State): Unit ={
    if(states.exists(_.name == state.name))
      throw new Exception(s"State ${state.name} defined multiple times, define a state only once")
  }

  /** Takes a method signature as a string and defines a transition from the state defined in an "in" statement above it
   *  to the state written after the goto statement after the signature.
   *  So a full line would look like: when("walk()") goto "State1"
   *  A method signature cannot be null.
   *
   * @param methodSignature method to transition between states
   * @return A Goto object which implements the goto function which should be used after this method
   */
  def when(methodSignature:String) = {
    checkProtocolHasNotBeenEnded()
    checkMethodSignatureIsValid(methodSignature)
    checkCurrentStateIsDefined()
    new Goto(methodSignature)
  }

  /** Checks if a method signature is null and throws an exception if so*/
  def checkMethodSignatureIsValid(methodSignature:String): Unit ={
    if(methodSignature == null)
      throw new Exception(s"You cannot call your method null. You called a method null in state $currentState")
  }

  /** Checks that a currentState is not null and if it is throws an exception.
   *  This is used so that a when statement is not used if a state has not been defined (with in) */
  def checkCurrentStateIsDefined(): Unit ={
    currentState match{
      case null =>
        throw new Exception("Defined a method without being inside a state. " +
          "Use in(\"stateName\") to define a state above a when(\"methodSignature\") statement")
      case _ =>
    }
  }


  class Goto(val methodSignature:String){
    currentMethod = getOrCreateMethod(methodSignature)
    methods += currentMethod
    def goto(nextState:String) ={
      var returnValue = getOrCreateReturnValue(Any)
      //Updates
      returnValues += returnValue
      transitions += Transition(currentState, currentMethod, returnValue, nextState)
      //initialise method set with index of its Any version (method:_Any_)
      if(currentMethod.indices.isEmpty) currentMethod.indices = Set(returnValueIndexCounter-1) //counter already incremented inside method above
      new At()
    }
  }

  /** Create a new method or fetch an exiting one with the same signature
   *
   * @param methodSignature
   * @return
   */
  def getOrCreateMethod(methodSignature:String):Method={
    var newMethod = Method(methodSignature, currentState)
    for(method <- methods){
      if(method.name == newMethod.name) {
        checkMethodIsOnlyDefinedOnceForTheCurrentState(method)
        newMethod = method
        newMethod.currentState = currentState
      }
    }
    newMethod
  }

  def checkMethodIsOnlyDefinedOnceForTheCurrentState(method:Method): Unit ={
    if(method.currentState == currentState)
      throw new Exception(s"Defined method ${method.name} for state $currentState more than once")
  }


  /** Creates a new return value or gets an existing one
   *
   * @return ReturnValue
   */
  def getOrCreateReturnValue(returnValue:String):ReturnValue={
    var newReturnValue = ReturnValue(currentMethod, returnValue, returnValueIndexCounter)
    returnValueIndexCounter+=1
    for(existingReturnValue <- returnValues){
      if(existingReturnValue.parentMethod.name == currentMethod.name && existingReturnValue.valueName == returnValue) {
        newReturnValue = existingReturnValue
        returnValueIndexCounter -=1 //put counter back down since it is not used this time
      }
    }
    newReturnValue
  }

  class At(){
    def at(returnValue:String)={
      checkReturnValueIsValid(returnValue)
      //corrects return value in transition just defined
      val lastTransition = transitions.last
      if(lastTransition.returnValue.valueName == Any) { //last transition was the first for this method
        //gets or creates a new return value so as not to overwrite the Any one
        var newReturnValue = getOrCreateReturnValue(returnValue)
        lastTransition.returnValue = newReturnValue
        returnValues += newReturnValue
      }
      else lastTransition.returnValue.valueName = returnValue

      currentMethod.indices += lastTransition.returnValue.index
      //replaces the transition with one with the correct return value
      transitions.dropRight(1)
      transitions.add(lastTransition)
      new Or()
    }
  }

  def checkReturnValueIsValid(returnValue:String): Unit ={
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
  }

  class Or(){
    def or(nextState:String) ={
      //create a new return value with undefined as the name and add a new Transition with it
      var returnValue = ReturnValue(currentMethod, Undefined, returnValueIndexCounter)
      transitions += Transition(currentState, currentMethod, returnValue, nextState)
      //Updates
      currentMethod.indices += returnValueIndexCounter
      returnValueIndexCounter +=1
      returnValues += returnValue
      new At()
    }
  }

  def end() = {
    checkWholeProtocolIsWellFormed()
    ended = true
    //create the array, print it and encode it into EncodedData.ser
    val arrayOfStates = createArrayOfStates()
    println(methods)
    println(returnValues)
    printNicely(arrayOfStates)
    sendDataToFile((arrayOfStates, sortSet(states).toArray, returnValues.toArray), "EncodedData.ser")
  }

  def checkWholeProtocolIsWellFormed(): Unit = {
    if (returnValues.exists(_.valueName == Undefined)) {
      val problematicTransition = transitions.filter(_.returnValue.valueName == Undefined).head
      throw new Exception(
        s"Defined state ${problematicTransition.nextState} to move to " +
          s"in state ${problematicTransition.startState} without defining a return value. " +
          s"Add an at after the or keyword")
    }
    if (!states.exists(_.index == 0))
      throw new Exception("No init state found in the protocol, make sure one of your states is called \"init\"")

    for (transition <- transitions) {
      if (!statesMap.contains(transition.nextState))
        throw new Exception(s"State ${transition.nextState}, " +
          s"used in state ${transition.startState} with method ${transition.method.name}, isn't defined")
    }
    if(ended) throw new Exception("You used end multiple times in the protocol, only use end once!")
  }

  def createArrayOfStates():Array[Array[State]] ={
    var arrayOfStates = Array[Array[State]]()
    arrayOfStates = Array.fill(states.size, returnValues.size)(State(Undefined, -1))
    transitions.foreach((transition:Transition) =>
      arrayOfStates(transition.startState.index)(transition.returnValue.index) = statesMap(transition.nextState))
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
    oos.close()
  }
}
