import java.io.{FileInputStream, ObjectInputStream}
import ProtocolDSL.{Method, ProtocolLang, ReturnValue, State}
import org.scalatest.{FlatSpec, Matchers}

class ProtocolLangTest extends FlatSpec with Matchers{
  "at init object" should "have correct values" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
      }
      assert (Test.stateIndexCounter === 0)
      assert (Test.returnValueIndexCounter === 0)
      assert (Test.currentState === null)
      assert (Test.currentMethod === null)
    }
  }

  "using in" should "create a state and add it to the statesMap" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
      }
      assert (Test.statesMap.contains("State0")  === true)
    }
  }

  "using in" should "create a state and add it to the states set" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
      }
      assert (Test.states.contains(State("State0", 0))  === true)
    }
  }

  "using in" should "increment the stateindexcounter" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
      }
      assert (Test.stateIndexCounter  === 1)
    }
  }

  "using in" should "give currentState a state" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
      }
      assert (Test.currentState  === State("State0", 0))
    }
  }

  "using in twice" should "add both states to the states set" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        in("State1")
      }
      assert (Test.states.contains(State("State0", 0))  === true)
      assert (Test.states.contains(State("State1", 1))  === true)
    }
  }

  "using when before using in" should "throw an error" in {
      object Test extends ProtocolLang {
        def main(args: Array[String]) = {
          a [Exception] should be thrownBy{
          when("walk(): Unit") goto "State0"
          in("State0")
        }
      }
    }
  }

  "transitioning to a non-defined state" should "throw an error" in {
    object Test extends ProtocolLang {
      def main(args: Array[String]) = {
        a [Exception] should be thrownBy{
          in("State0")
          when("walk(): Unit") goto "State1"
        }
      }
    }
  }

  "using when" should "add the correct transition" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk(): Unit") goto "State0"
      }
      assert (Test.transitions.contains(
        Transition(
          State("State0", 0),
          Method("walk(): Unit", null),
          ReturnValue(Method("walk(): Unit", null), null, 0),
            "State0")
      )  === true)
    }
  }

  "using end" should "create the matrix" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk(): Unit") goto "State0"
        end
      }
      assert(Test.arrayOfStates(0)(0) === State("State0", 0) )
    }
  }

  "using or" should "create a correct entry in the transitions list" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk(): Boolean") goto "State0" at "True" or "State1" at "False"
        end
      }
      assert(Test.transitions.contains(
        Transition(
          State("State0", 0),
          Method("walk(): Boolean", Set(0,1)),
          ReturnValue(Method("walk(): Boolean", Set(0,1)), "True", 0),
          "State0"
        )
      ))
    }
  }

  "using or" should "create a correct entry in the matrix list" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk()") goto "State0" at "True" or "State1" at "False"
        in ("State1")
        end
      }
      assert(Test.arrayOfStates(0)(1) === State("State1", 1))
    }
  }

  "using the correct syntax" should "create an array of the right dimensions" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when("walk():Unit") goto "walking"
        when("cry():Unit") goto "crying"
        when("laze():Unit") goto "lazing"
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in ("walking")
        when("cry():Unit") goto "crying"

        in("crying")
        when("laze():Unit") goto "lazing"

        in("lazing")
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in("onAlert")
        when("laze():Unit") goto "lazing"
        end()
      }
      assert(Test.arrayOfStates.size === 4)
      assert(Test.arrayOfStates(0).size === 5)
    }
  }

  "writing in a certain number of states" should "create a set of the right size" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when("walk():Unit") goto "walking"
        when("cry():Unit") goto "crying"
        when("laze():Unit") goto "lazing"
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in ("walking")
        when("cry():Unit") goto "crying"

        in("crying")
        when("laze():Unit") goto "lazing"

        in("lazing")
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in("onAlert")
        when("laze():Unit") goto "lazing"
        end()
      }
      assert(Test.states.size === 5)
    }
  }

  "writing in a certain number of methods" should "create a set of the right size" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when("walk():Unit") goto "walking"
        when("cry():Unit") goto "crying"
        when("laze():Unit") goto "lazing"
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in ("walking")
        when("cry():Unit") goto "crying"

        in("crying")
        when("laze():Unit") goto "lazing"

        in("lazing")
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in("onAlert")
        when("laze():Unit") goto "lazing"
        end()
      }
      assert(Test.methods.size === 4)
    }
  }

  "writing in a certain number of transitions" should "create a set of the right size" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when("walk():Unit") goto "walking"
        when("cry():Unit") goto "crying"
        when("laze():Unit") goto "lazing"
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in ("walking")
        when("cry():Unit") goto "crying"

        in("crying")
        when("laze():Unit") goto "lazing"

        in("lazing")
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in("onAlert")
        when("laze():Unit") goto "lazing"
        end()
      }
      assert(Test.transitions.size === 8)
    }
  }

  "writing in a certain number of return values" should "create a set of the right size" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when("walk():Unit") goto "walking"
        when("cry():Unit") goto "crying"
        when("laze():Unit") goto "lazing"
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in ("walking")
        when("cry():Unit") goto "crying"

        in("crying")
        when("laze():Unit") goto "lazing"

        in("lazing")
        when("stayOnAlert(Boolean):Unit") goto "onAlert"

        in("onAlert")
        when("laze():Unit") goto "lazing"
        end()
      }
      assert(Test.returnValues.size === 4)
    }
  }

  "writing in a certain number of transitions with or statements" should "create a set of the right size" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when ("walk()") goto "State3"
        when("comeAlive") goto "State1"
        when ("comeAlive(String, Int): String") goto "init"
        when ("die: DeathState") goto
          "State1" at "Dead" or
          "State2" at "Alive" or
          "State3" at "Unsure" or
          "State1" at null

        in ("State3")
        when("run(): Unit") goto "State2"
        in ("State2")
        in ("State1")
        end()
      }
      assert(Test.transitions.size === 8)
    }
  }

  "writing in a certain number of return values with or statements" should "create a set of the right size" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in ("init")
        when ("walk()") goto "State3"
        when("comeAlive") goto "State1"
        when ("comeAlive(String, Int): String") goto "init"
        when ("die: DeathState") goto
          "State1" at "Dead" or
          "State2" at "Alive" or
          "State3" at "Unsure" or
          "State1" at null

        in ("State3")
        when("run(): Unit") goto "State2"
        in ("State2")
        in ("State1")
        end()
      }
      assert(Test.returnValues.size === 8)
    }
  }

  "using end" should "serialize the data into a file such that it can be decoded" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk()") goto "State0" at "True" or "State1" at "False"
        in ("State1")
        end
      }
      val (stateMatrix, stateArray, returnValueArray) = getDataFromFile("EncodedData.ser")
      assert(stateMatrix(0)(0) == State("State0", 0))
      assert(stateArray(0) == State("State0", 0))
      assert(returnValueArray(0) == ReturnValue(Method("walk()", Set(0,1)), "True", 0))
    }
  }

  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close
    stock
  }
}
