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

  //region<copyInstances>
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
  //endregion

  //region<mergeInstanceStates>
  "merging two empty sets" should "create one empty set" in {
    val firstSet:Set[Instance] = Set()
    val secondSet:Set[Instance] = Set()
    assert(mergeInstanceStates(firstSet, secondSet) == Set())
  }

  "merging one set with an empty one" should "give back the one set" in {
    val firstSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    val secondSet:Set[Instance] = Set()
    assert(mergeInstanceStates(firstSet, secondSet) == firstSet)
  }

  "merging an empty set with nonempty" should "give back the nonempty set" in {
    val firstSet:Set[Instance] = Set()
    val secondSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    assert(mergeInstanceStates(firstSet, secondSet) == secondSet)
  }

  "merging two sets with the same aliases" should "give back the first or second set" in {
    val firstSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    val secondSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    assert(mergeInstanceStates(firstSet, secondSet) == secondSet)
  }

  "merging two sets with the same aliases in different states" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    val secondSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("S1", 1))))
    val mergedSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("S1", 1), State("init", 0))))
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with different aliases in different states" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    val secondSet:Set[Instance] = Set(Instance("myClass", Set(Alias("yourAlias", mutable.Stack("main"))), Set(State("S1", 1))))
    val mergedSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))),
      Instance("myClass", Set(Alias("yourAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with some same aliases in different states" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))))
    val secondSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("S1", 1))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    val mergedSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0), State("S1", 1))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with some same aliases in different states, extra in first" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    val secondSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("S1", 1))),
    )
    val mergedSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0), State("S1", 1))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with some same aliases in different states, extra in both" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    val secondSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("S1", 1))),
      Instance("myClass", Set(Alias("yetAnotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    val mergedSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0), State("S1", 1))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1))),
      Instance("myClass", Set(Alias("yetAnotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with multiple alises to merge" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S2", 1)))
    )
    val secondSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("S1", 1))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1)))
    )
    val mergedSet:Set[Instance] = Set(
      Instance("myClass", Set(Alias("myAlias", mutable.Stack("main"))), Set(State("init", 0), State("S1", 1))),
      Instance("myClass", Set(Alias("anotherAlias", mutable.Stack("main"))), Set(State("S1", 1),State("S2", 1)))
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }
  //endregion
}
