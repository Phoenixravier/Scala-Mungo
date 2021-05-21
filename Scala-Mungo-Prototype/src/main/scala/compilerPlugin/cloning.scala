package compilerPlugin

import ProtocolDSL.State

import scala.collection.mutable

class cloning {

}

object Main extends App{
  var alias1 = Alias("myal", mutable.Stack("scope"))
  var alias2 = Alias(alias1.name, alias1.scope)
  alias1.name = "Alice"
  println(alias2)

  var states1 = Set(State("sta", 1))
  var states2 = State(states1.last.name, states1.last.index)
  states1 = Set(State("SSS", 2))
  println(states2)

  var fields1 = mutable.Map(Alias("al", mutable.Stack("scope")) -> Set[Instance]())
  var fields2 = mutable.Map[Alias, Set[Instance]]()
  for((key, value) <- fields1)
    fields2 += (Alias(key.name, key.scope) -> value)
  fields1.last._1.name = "Sofia"
  println(fields2)

  var inst1 = Instance(Alias("ia", mutable.Stack("scope")), Set(State("stai", 2)), mutable.Map[Alias, Set[Instance]](), 0)
  var inst2 = Instance(Alias(inst1.alias.name, inst1.alias.scope), inst1.currentStates, mutable.Map[Alias, Set[Instance]](), inst1.id)
  inst1.alias.name = "Piotrus"
  inst1.currentStates ++= Set(State("STATE", 3))
  inst1.fields += (Alias("someal", mutable.Stack("depth")) -> Set[Instance]())
  println(inst2)
  println(inst1)

  var someMap = mutable.Map("haha" -> "hehe")
  println(someMap.contains("haha"))


  var firstInstances = Set(compilerPlugin.Instance(
    compilerPlugin.Alias("cat", mutable.Stack("here")),Set(State("state1", 1)), mutable.Map[Alias, Set[Instance]](), 1))
  var elemInfo1 = ElementInfo(null, null, null, null, null, firstInstances)
  var map1 = mutable.Map("someType" -> elemInfo1)


  var secondInstances = Set(compilerPlugin.Instance(
    compilerPlugin.Alias("cat", mutable.Stack("here")),Set(State("state2", 2)), mutable.Map[Alias, Set[Instance]](), 1))
  var elemInfo2 = ElementInfo(null, null, null, null, null, secondInstances)
  var map2 = mutable.Map("someType" -> elemInfo2)

  var ds = mutable.Map(map1 -> map2)

  println(ds.contains(map2))

}