import java.io.{FileInputStream, ObjectInputStream}

import ProtocolDSL.{Method, ProtocolLang, ReturnValue, State, Transition}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class ProtocolLangTest extends FlatSpec with Matchers with BeforeAndAfter{
  val Any = "_Any_"
  val Undefined = "_Undefined_"


  "at init, object pl" should "have correct values" in {
    val pl = new ProtocolLang
    assert (pl.stateIndexCounter === 1)
    assert (pl.returnValueIndexCounter === 0)
    assert (pl.currentState === null)
    assert (pl.currentMethod === null)
  }


  "using in" should "create a state and add it to the statesMap" in {
    val pl = new ProtocolLang
    pl in("init")
    assert (pl.statesMap.contains("init")  === true)
  }

  "using in" should "create a state and add it to the states set" in {
    val pl = new ProtocolLang
    pl in("init")
    assert (pl.states.contains(State("init", 0))  === true)
  }

  "using in" should "increment the stateindexcounter" in {
    val pl = new ProtocolLang
    pl in("init")
    assert (pl.stateIndexCounter  === 1)
  }

  "using in" should "give currentState a state" in {
    val pl = new ProtocolLang
    pl in("init")
    assert (pl.currentState  === State("init", 0))
  }

  "using in twice" should "add both states to the states set" in {
    val pl = new ProtocolLang
    pl in("init")
    pl in("State1")
    assert (pl.states.contains(State("init", 0))  === true)
    assert (pl.states.contains(State("State1", 1))  === true)
  }

  "using when" should "add the correct transition" in {
    val pl = new ProtocolLang
    pl in("init")
    pl when("walk(): Unit") goto "init"
    val transition = Transition(
      State("init", 0),
      Method("walk(): Unit", State("init", 0), Set(0)),
      ReturnValue(Method("walk(): Unit", State("init", 0),Set(0)), Any, 0),
      "init"
    )
    assert (pl.transitions.contains(transition) === true)
  }

  "using end" should "create the matrix" in {
    val pl = new ProtocolLang
    pl in "init"
    pl when "walk(): Unit" goto "init"
    pl.end()
    assert(pl.arrayOfStates(0)(0) === State("init", 0))
  }

  "using or" should "create a correct entry in the transitions list" in {
    val pl = new ProtocolLang
    pl in("init")
    pl when("walk(): Boolean") goto "init" at "True" or "State1" at "False"
    pl in("State1")
    pl.end()
    assert(pl.transitions.contains(
      Transition(
        State("init", 0),
        Method("walk(): Boolean", State("init",0),Set(0,1)),
        ReturnValue(Method("walk(): Boolean", State("init",0), Set(0,1)), "True", 0),
        "init"
      )
    ))
  }

  "using or" should "create a correct entry in the matrix list" in {
    val pl = new ProtocolLang
    pl in("init")
    pl when("walk()") goto
      "init" at "True" or
      "State1" at "False"
    pl in ("State1")
    pl.end()
    assert(pl.arrayOfStates(0)(2) === State("State1", 1))
  }

  "using end" should "serialize the data into a file such that it can be decoded" in {
    val pl = new ProtocolLang
    pl in("init")
    pl when("walk()") goto
      "init" at "True" or
      "State1" at "False"
    pl in ("State1")
    pl.end()
    val (stateMatrix, stateArray, returnValueArray) = getDataFromFile("EncodedData.ser")
    assert(stateMatrix(0)(1) == State("init", 0))
    assert(stateArray(0) == State("init", 0))
    assert(returnValueArray(1) == ReturnValue(Method("walk()", State("init",0), Set(0,1)), "True", 0))
  }

  "using the correct syntax" should "create an array of the right dimensions" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when("walk():Unit") goto "walking"
    pl when("cry():Unit") goto "crying"
    pl when("laze():Unit") goto "lazing"
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in ("walking")
    pl when("cry():Unit") goto "crying"

    pl in("crying")
    pl when("laze():Unit") goto "lazing"

    pl in("lazing")
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in("onAlert")
    pl when("laze():Unit") goto "lazing"
    pl.end()
    assert(pl.arrayOfStates.size === 5)
    assert(pl.arrayOfStates(0).size === 4)
  }

  "writing in a certain number of states" should "create a set of the right size" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when("walk():Unit") goto "walking"
    pl when("cry():Unit") goto "crying"
    pl when("laze():Unit") goto "lazing"
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in ("walking")
    pl when("cry():Unit") goto "crying"

    pl in("crying")
    pl when("laze():Unit") goto "lazing"

    pl in("lazing")
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in("onAlert")
    pl when("laze():Unit") goto "lazing"
    pl.end()
    assert(pl.states.size === 5)
  }

  "writing in a certain number of methods" should "create a set of the right size" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when("walk():Unit") goto "walking"
    pl when("cry():Unit") goto "crying"
    pl when("laze():Unit") goto "lazing"
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in ("walking")
    pl when("cry():Unit") goto "crying"

    pl in("crying")
    pl when("laze():Unit") goto "lazing"

    pl in("lazing")
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in("onAlert")
    pl when("laze():Unit") goto "lazing"
    pl.end()
    assert(pl.methods.size === 4)
  }

  "writing in a certain number of transitions" should "create a set of the right size" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when("walk():Unit") goto "walking"
    pl when("cry():Unit") goto "crying"
    pl when("laze():Unit") goto "lazing"
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in ("walking")
    pl when("cry():Unit") goto "crying"

    pl in("crying")
    pl when("laze():Unit") goto "lazing"

    pl in("lazing")
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in("onAlert")
    pl when("laze():Unit") goto "lazing"
    pl.end()
    assert(pl.transitions.size === 8)
  }

  "writing in a certain number of return values" should "create a set of the right size" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when("walk():Unit") goto "walking"
    pl when("cry():Unit") goto "crying"
    pl when("laze():Unit") goto "lazing"
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in ("walking")
    pl when("cry():Unit") goto "crying"

    pl in("crying")
    pl when("laze():Unit") goto "lazing"

    pl in("lazing")
    pl when("stayOnAlert(Boolean):Unit") goto "onAlert"

    pl in("onAlert")
    pl when("laze():Unit") goto "lazing"
    pl.end()
    assert(pl.returnValues.size === 4)
  }

  "writing in a certain number of transitions with or statements" should "create a set of the right size" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when ("walk()") goto "State3"
    pl when("comeAlive") goto "State1"
    pl when ("comeAlive(String, Int): String") goto "init"
    pl when ("die: DeathState") goto
      "State1" at "Dead" or
      "State2" at "Alive" or
      "State3" at "Unsure" or
      "State1" at null

    pl in ("State3")
    pl when("run(): Unit") goto "State2"
    pl in ("State2")
    pl in ("State1")
    pl.end()

    assert(pl.transitions.size === 8)
  }

  "writing in a certain number of return values with or statements" should "create a set of the right size" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when ("walk()") goto "State3"
    pl when("comeAlive") goto "State1"
    pl when ("comeAlive(String, Int): String") goto "init"
    pl when ("die: DeathState") goto
      "State1" at "Dead" or
      "State2" at "Alive" or
      "State3" at "Unsure" or
      "State1" at null

    pl in ("State3")
    pl when("run(): Unit") goto "State2"
    pl in ("State2")
    pl in ("State1")
    pl.end()
    assert(pl.returnValues.size === 9)
  }

  "writing in the same method twice with different return types" should "only add the return value objects once" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when ("walk()") goto "State3"
    pl when("comeAlive") goto "State1"
    pl when ("comeAlive(String, Int): String") goto "init"
    pl when ("die: DeathState") goto
      "State1" at "Dead" or
      "State2" at "Alive" or
      "State3" at "Unsure" or
      "State1" at null

    pl in ("State3")
    pl when("run(): Unit") goto "State2"
    pl in ("State2")
    pl when("die: DeathState") goto
      "State1" at "Dead"
    pl in ("State1")
    pl.end()
    assert(pl.returnValues.size === 9)
    assert(pl.arrayOfStates(2)(4) === State("State1",3))
  }

  "creating a method" should "create a version with the Any return value" in {
    val pl = new ProtocolLang
    pl in ("init")
    pl when ("walk()") goto "State3"
    pl when("comeAlive") goto "State1"

    pl in ("State3")
    pl in ("State2")
    pl when("walk()") goto
      "State1" at "Dead"
    pl in ("State1")
    pl.end()
    assert(pl.returnValues.size === 3)
    assert(pl.arrayOfStates(0).size === 3)
  }


  /*------------------------
   * EXCEPTIONS
   * -----------------------
   */

  "using when before using in" should "throw an error" in {
    val pl = new ProtocolLang
    the [Exception] thrownBy{
      pl when("walk(): Unit") goto "init"
      pl in("init")
    } should have message("Defined a method without being inside a state. " +
      "Use in(State) to define a state above a when(method) statement")
  }

  "transitioning to a non-defined state" should "throw an error" in {
    val pl = new ProtocolLang
    the [Exception] thrownBy{
      pl in("init")
      pl when("walk(): Unit") goto "State1"
      pl.end()
    } should have message("State State1, used in state init with method walk(): Unit, isn't defined")
  }

  "not defining an init state" should "throw an exception" in {
    val pl = new ProtocolLang
    the [Exception] thrownBy {
      pl in("State0")
      pl when("walk()") goto "State0" at "True" or "State1" at "False"
      pl in("State1")
      pl.end()
    } should have message("No init state found in the protocol, make sure one of your states is called \"init\"")
  }

  "defining init multiple times" should "throw an exception" in {
    val exception = intercept[Exception]{
      val pl = new ProtocolLang
      pl in("init")
      pl when("walk()") goto "init" at "True" or "State1" at "False"
      pl in("init")
      pl.end()
    }
    assert(exception.getMessage === "State init defined multiple times, define a state only once")
  }

  "defining a state which is not init multiple times" should "throw an exception" in {
    val exception = intercept[Exception]{
      val pl = new ProtocolLang
      pl in("init")
      pl in("State1")
      pl in("State1")
      pl.end()
    }
    assert(exception.getMessage === "State State1 defined multiple times, define a state only once")
  }

  "Using or without at" should "throw an exception" in {
    val exception = intercept[Exception]{
      val pl = new ProtocolLang
      pl in("init")
      pl when("walk()") goto "init" at "True" or "State1"
      pl in("State1")
      pl.end()
    }
    assert(exception.getMessage === "Defined state State1 to move to in state init without defining a return value. Add an at after the or keyword")
  }

  s"Trying to call a state $Undefined" should "throw an exception" in {
    val exception = intercept[Exception]{
      val pl = new ProtocolLang
      pl in("_Undefined_")
      pl when("walk()") goto "init" at "True" or "State1"
      pl in("State1")
      pl.end()
    }
    assert(exception.getMessage === s"You cannot call a state $Undefined")
  }

  s"Trying to call a return value $Any" should "throw an exception" in {
    val exception = intercept[Exception]{
      val pl = new ProtocolLang
      pl in("init")
      pl when("walk()") goto "init" at "_Any_" or "State1"
      pl in("State1")
      pl.end()
    }
    assert(exception.getMessage === s"You used $Any in state init as a return value for method walk()." +
      s"It is not allowed to use $Any or $Undefined as return values for a method")
  }

  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close
    stock
  }
}
