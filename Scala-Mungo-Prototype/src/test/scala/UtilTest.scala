
import ProtocolDSL.State
import compilerPlugin.{Alias, ElementInfo, Instance}
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

  //region addFields
  "same field in two maps with different states" should "give field with both states" in {
    val scope = mutable.Stack("type")
    val firstCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("init", 0)), mutable.Map[Alias, Set[Instance]](), 0)
    val secondCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("end", 1)), mutable.Map[Alias, Set[Instance]](), 0)
    val endCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("init", 0), State("end", 1)), mutable.Map[Alias, Set[Instance]](), 0)
    val firstFields = mutable.Map(Alias("cat", scope) -> Set(firstCatInstance))
    val secondFields = mutable.Map(Alias("cat", scope) -> Set(secondCatInstance))
    val firstInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, firstFields, 0), firstCatInstance)
    val secondInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, secondFields, 0), secondCatInstance)
    val newInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, mutable.Map[Alias, Set[Instance]]()), endCatInstance)
    val firstMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, firstInstances))
    val secondMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, secondInstances))
    val newMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, newInstances))
    println(newMap)
    println()

    val finalCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("init", 0), State("end", 1)), mutable.Map[Alias, Set[Instance]](), 0)
    val finalFields = mutable.Map(Alias("cat", scope) -> Set(finalCatInstance))
    val finalInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, finalFields), finalCatInstance)
    val finalMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, finalInstances))
    println(finalMap)
    addFields(firstMap, secondMap, newMap) should be (finalMap)
  }

  //endregion

  //region mergeMaps
  "merging two maps" should "merge their instance states and field pointers" in {
    val scope = mutable.Stack("type")
    val firstCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("init", 0)), mutable.Map[Alias, Set[Instance]](), 0)
    val secondCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("end", 1)), mutable.Map[Alias, Set[Instance]](), 0)
    val firstFields = mutable.Map(Alias("cat", scope) -> Set(firstCatInstance))
    val secondFields = mutable.Map(Alias("cat", scope) -> Set(secondCatInstance))
    val firstInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, firstFields, 0), firstCatInstance)
    val secondInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, secondFields, 0), secondCatInstance)
    val firstMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, firstInstances))
    val secondMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, secondInstances))

    val endCatInstance = Instance(Alias("compilerPlugin.Cat", scope), Set(State("init", 0), State("end", 1)), mutable.Map[Alias, Set[Instance]](), 0)
    val newInstances = Set(Instance(Alias("compilerPlugin.Main", scope), null, mutable.Map[Alias, Set[Instance]]()), endCatInstance)
    val newMap = mutable.Map("compilerPlugin.Cat" -> ElementInfo(null, null, null, null, null, newInstances))
    mergeMaps(firstMap, secondMap) should be (newMap)
  }
  //endregion

  /*
  //region<copyInstances>
  "copying instances" should "create separate sets and editing one alias name should not edit the other" in {
    var firstSet = Set(compilerPlugin.Instance(Set(compilerPlugin.Alias("cat", mutable.Stack("here"))),
      Set(ProtocolDSL.State("state1", 1)), mutable.Map()))
    var secondInstances= copyInstances(firstSet)
    firstSet.last.aliases.last.name = "kitty"
    assert(secondInstances.last.aliases.last.name == "cat")
  }

  "copying instances" should "create separate sets and editing one set should not edit the other" in {
    var firstSet:Set[Instance] = Set(compilerPlugin.Instance(Set(compilerPlugin.Alias("cat", mutable.Stack("here"))),
      Set(ProtocolDSL.State("state1", 1)), mutable.Map()))
    var secondInstances= copyInstances(firstSet)
    firstSet+= Instance(null,null, mutable.Map())
    assert(secondInstances.last.aliases.last.name == "cat")
    assert(secondInstances.size == 1)
  }
  //endregion
  */

  //region<mergeInstanceStates>
  "merging two empty sets" should "create one empty set" in {
    val firstSet:Set[Instance] = Set()
    val secondSet:Set[Instance] = Set()
    assert(mergeInstanceStates(firstSet, secondSet) == Set())
  }

  "merging one set with an empty one" should "give back the one set" in {
    val firstSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    val secondSet:Set[Instance] = Set()
    assert(mergeInstanceStates(firstSet, secondSet) == firstSet)
  }

  "merging an empty set with nonempty" should "give back the nonempty set" in {
    val firstSet:Set[Instance] = Set()
    val secondSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    assert(mergeInstanceStates(firstSet, secondSet) == secondSet)
  }

  "merging two sets with the same aliases" should "give back the first or second set" in {
    val firstSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    val secondSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    assert(mergeInstanceStates(firstSet, secondSet) == secondSet)
  }

  "merging two sets with the same aliases in different states" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    val secondSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()))
    val mergedSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("S1", 1), State("init", 0)), mutable.Map()))
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with different aliases in different states" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    val secondSet:Set[Instance] = Set(Instance(Alias("yourAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()))
    val mergedSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()),
      Instance(Alias("yourAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with some same aliases in different states" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()))
    val secondSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    val mergedSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0), State("S1", 1)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with some same aliases in different states, extra in first" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    val secondSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()),
    )
    val mergedSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0), State("S1", 1)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with some same aliases in different states, extra in both" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    val secondSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()),
      Instance(Alias("yetAnotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    val mergedSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0), State("S1", 1)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()),
      Instance(Alias("yetAnotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }

  "merging two sets with multiple alises to merge" should "give back a merged set" in {
    val firstSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S2", 1)), mutable.Map())
    )
    val secondSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1)), mutable.Map())
    )
    val mergedSet:Set[Instance] = Set(
      Instance(Alias("myAlias", mutable.Stack("main")), Set(State("init", 0), State("S1", 1)), mutable.Map()),
      Instance(Alias("anotherAlias", mutable.Stack("main")), Set(State("S1", 1),State("S2", 1)), mutable.Map())
    )
    assert(mergeInstanceStates(firstSet, secondSet) == mergedSet)
  }
  //endregion

}


