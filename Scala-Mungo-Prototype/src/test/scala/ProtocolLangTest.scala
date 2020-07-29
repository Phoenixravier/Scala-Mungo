import ProtocolDSL.Example.{in, when}
import ProtocolDSL.ProtocolLang
import org.scalatest.{FlatSpec, Matchers}

class ProtocolLangTest extends FlatSpec with Matchers{
  "at init object" should "have correct values" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
      }
      assert (Test.stateIndexCounter === 0)
      assert (Test.methodIndexCounter === 0)
      assert (Test.currentState === null)
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
      assert (Test.transitions.contains(Transition(State("State0", 0), Method("walk(): Unit", 0), "State0"))  === true)
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

  "using when" should "create a correct entry in the methodsMap" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk(): Unit") goto "State0"
        end
      }
      assert(Test.methodsMap("walk(): Unit") === 0 )
    }
  }

  "using or" should "create a correct entry in the transitions list" in {
    object Test extends ProtocolLang{
      def main(args:Array[String]) = {
        in("State0")
        when("walk(): Boolean") goto "State0" at "True" or "State1" at "False"
        end
      }
      assert(Test.methodsMap("walk(): Unit") === 0 )
    }
  }
}
