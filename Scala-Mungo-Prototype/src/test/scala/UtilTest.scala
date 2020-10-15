import ProtocolDSL.State
import compilerPlugin.{Alias, Instance}
import compilerPlugin.Util._
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

import scala.collection.mutable

class UtilTest extends FlatSpec with Matchers with BeforeAndAfter{

  //region<stripReturnValue>
  "stripping return value from walk(Int)" should "give walk(Int)" in {
    stripReturnValue("walk(Int)") should be ("walk(Int)")
  }

  "stripping return value from walk()" should "give walk()" in {
    stripReturnValue("walk()") should be ("walk()")
  }

  "stripping return value from walk(Int):" should "give walk(Int)" in {
    stripReturnValue("walk(Int):") should be ("walk(Int)")
  }

  "stripping return value from walk(Int):Int" should "give walk(Int)" in {
    stripReturnValue("walk(Int):Int") should be ("walk(Int)")
  }

  "stripping return value from walk" should "give walk()" in {
    stripReturnValue("walk") should be ("walk()")
  }

  "stripping return value from walk():Int" should "give walk()" in {
    stripReturnValue("walk():Int") should be ("walk()")
  }

  "stripping return value from walk:" should "give walk()" in {
    stripReturnValue("walk:") should be ("walk()")
  }
  //endregion

  "copying instances" should "create separate sets and editing one alias name should not edit the other" in {
    var firstSet = Set(compilerPlugin.Instance("Cat", Set(compilerPlugin.Alias("cat", mutable.Stack("here"))),Set(ProtocolDSL.State("state1", 1))))
    var secondInstances= copyInstances(firstSet)
    firstSet.last.aliases.last.name = "kitty"
    assert(secondInstances.last.aliases.last.name == "cat")
  }

  "copying instances" should "create separate sets and editing one set should not edit the other" in {
    var firstSet:Set[Instance] = Set(compilerPlugin.Instance("Cat", Set(compilerPlugin.Alias("cat", mutable.Stack("here"))),Set(ProtocolDSL.State("state1", 1))))
    var secondInstances= copyInstances(firstSet)
    firstSet+= Instance(null,null,null)
    assert(secondInstances.last.aliases.last.name == "cat")
    assert(secondInstances.size == 1)
  }
}
