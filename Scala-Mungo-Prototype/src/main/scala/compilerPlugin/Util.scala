package compilerPlugin

import java.io.{FileInputStream, ObjectInputStream}

import ProtocolDSL.{ReturnValue, State}

import scala.sys.process._
import scala.collection.{SortedSet, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.api.Trees

object Util {
  val Undefined = "_Undefined_"
  val Unknown = "_Unknown_"
  var currentScope:mutable.Stack[String] = mutable.Stack()

  /** Returns protocol data from a file */
  def getDataFromFile(filename: String): (Array[Array[State]], Array[State], Array[ReturnValue]) ={
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val stock = ois.readObject.asInstanceOf[(Array[Array[State]], Array[State], Array[ReturnValue])]
    ois.close
    stock
  }

  /** Removes all instances with an empty set of aliases */
  def cleanInstances(instances:Set[Instance]): Set[Instance]={
    println("instances before cleaning are "+instances)
    var newInstances = for(instance <- instances) yield instance
    for(instance <- newInstances if instance.aliases.isEmpty) newInstances -= instance
    println("instances after cleaning are "+newInstances)
    newInstances
  }

  /** Checks to see if there are duplicates in all the lists of a map(Instance -> list) */
  def duplicatesInAllListsOfMap(map:mutable.HashMap[Instance, ListBuffer[Set[State]]]):Boolean={
    for((instance, list) <- map) for((instance, list) <- map if list.diff(list.distinct).isEmpty) return false
    true
  }


  /** Sorts a set */
  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] = SortedSet.empty[A] ++ unsortedSet


  /** Compiles the user protocol and the necessary classes(ProtocolLang), then executes the protocol,
   * generating the serialised data */
  def executeFile(filename:String): Unit ={
    println(filename)
    var className = filename.substring(filename.indexOf("\"")+1)
    println(className)
    className = className.substring(className.lastIndexOf("\\")+1, className.lastIndexOf("."))
    println(className)
    s"executeUserProtocol.bat $filename $className".!
  }

  /** Gets rid of the return value in a method name string and keeps the parenthesis at the end */
  def stripReturnValue(methodName:String): String ={
    println("inside strip value")
    //case walk -> walk()
    if(!(methodName.contains(':') || methodName.contains("()") || (methodName.contains("(") && methodName.contains(")")))){
      println("matched first")
      methodName+"()"
    }
      //cases walk:Int and walk: -> walk()
    else if(methodName.contains(':') && !methodName.contains("(") && !methodName.contains(")")) methodName.substring(0,methodName.indexOf(':'))+"()"
    //cases walk() and walk(Int) -> walk() and walk(Int)
    else if(methodName(methodName.length-1) == ')') methodName
      //cases walk():Int and walk(Int):Int -> walk() and walk(Int)
    else methodName.substring(0,methodName.indexOf(')')+1)
  }

  /** Takes a string a strips everything after, and including ( from it */
  def keepOnlyMethodName(method:String): String ={
    if(method.contains('(')) method.substring(0,method.indexOf('('))
    else method
  }

  /** Prints something easy to see while debugging, use above an interesting print statement */
  def printBanner(): Unit ={
    println("------------------")
    println("LOOK HERE")
    println("------------------")
  }

  /** Just a dummy function to check an object's type */
  def ckType(s:String): Unit ={
    println("hi")
  }

  /** Gets the length of a tree in nb of lines */
  def getLengthOfTree(tree:Trees#Tree): Int ={
    var length = 0
    for(line <- tree) length +=1
    length
  }

  /** Searches through instances for an instance with an empty set and adds the alias there */
  def addInMissingAlias(instances: Set[Instance], name: String):Set[Instance] = {
    val newInstances = for(instance <- instances) yield instance
    for(instance <- newInstances if instance.aliases.isEmpty) instance.aliases += Alias(name, currentScope.clone)
    newInstances
  }

  /** Creates a hashmap from method names (e.g. "walk(String)")
   * to indices at which it is present in the transitions array (including all the return values it might have)
   *
   * @param returnValuesArray:Array[ReturnValue]
   * @return mutable.Hashmap[String, Set[Int]]
   */
  def createMethodToIndicesMap(returnValuesArray:Array[ReturnValue]): mutable.HashMap[String, Set[Int]] ={
    var methodToIndices:mutable.HashMap[String, Set[Int]] = mutable.HashMap()
    for(returnValue <- returnValuesArray){
      methodToIndices += (Util.stripReturnValue(returnValue.parentMethod.name) -> returnValue.parentMethod.indices)
    }
    methodToIndices
  }

  /** From a scope implemented as a stack, gets a string formatted with dots */
  def getScopeString(scopeStack:mutable.Stack[String]): String ={
    scopeStack.reverse.mkString(".")
  }

  def removeAliasesInScope(instances: Set[Instance], aliasName: String, scope:mutable.Stack[String]): Set[Instance] = {
    var newInstances = for (instance <- instances) yield instance
    val instancesToUpdate = newInstances.filter(instance => instance.containsAliasInfo(aliasName, scope))
    for (instance <- instancesToUpdate)
      instance.aliases -= Alias(aliasName, scope)
    Util.cleanInstances(newInstances)
  }



  def printDragon(): Unit ={
    println(
      """
        |                                        ,   ,
        |                                        $,  $,     ,
        |                                        "ss.$ss. .s'
        |                                ,     .ss$$$$$$$$$$s,
        |                                $. s$$$$$$$$$$$$$$`$$Ss
        |                                "$$$$$$$$$$$$$$$$$$o$$$       ,
        |                               s$$$$$$$$$$$$$$$$$$$$$$$$s,  ,s
        |                              s$$$$$$$$$"$$$$$$\"\"\"\"$$$$$$"$$$$$,
        |                              s$$$$$$$$$$s""$$$$ssssss"$$$$$$$$"
        |                             s$$$$$$$$$$'         `\"\"\"ss"$"$s"\"
        |                             s$$$$$$$$$$,              `\"\"\"\"\" $.s$$s
    | s$$$$$$$$$$$$s
    ,...`s$$'  `
    |
    ` ssss$$$$$$$$$$$$$$$$$$$$ #### s..
    $$"$.   , s-
      | ` \ "\"\"" $$$$$$$$$$$$$$$$$$$$ ##### $$$$$$"     $.$'
      | "$$$$$$$$$$$$$$$$$$$$$####s""     .$$$|
 | "$$$$$$$$$$$$$$$$$$$$$$$$##s    .$$" $
      | $$""
    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$"   `
      | $$"  "
    $"$$$$$$$$$$$$$$$$$$$$S\"\"\""
    '
    |
    ,, "     '  $$$$$$$$$$$$$$$$####s
    | $
    ..s$$$$$$$$$$$$$$$$$ #### "
    |
    , "$s.   ..ssS$$$$$$$$$$$$$$$$$$$####"
    | $
    .$$$S$$$$$$$$$$$$$$$$$$$$$$$$ ##### "
    | Ss
    ..sS$$$$$$$$$$$$$$$$$$$$$$$$$$$ ###### ""
    |
    "$$sS$$$$$$$$$$$$$$$$$$$$$$$$$$$########"
    |
    , s$$$$$$$$$$$$$$$$$$$$$$$$ ######### ""
    '
    | $ s$$$$$$$$$$$$$$$$$$$$$ ####### ""
    ' s
    ',
    | $$
    ..$$$$$$$$$$$$$$$$$$ ###### "'       ....,$$....    ,$
    |
    "$$$$$$$$$$$$$$$######"
    ',.sS$$$$$$$$$$$$$$$$s$$
    | $$$$$$$$$$$$ #####
    "     $, .s$$$$$$$$$$$$$$$$$$$$$$$$s.
    |
    ) $$$$$$$$$$$ #####
    ' ` $$$$$$$$$ ########### $$$$$$$$$$$.
      |(($$$$$$$$$$$ ##### $$$$$$$$ ### "       " #### $$$$$$$$$$
        |) \ $$$$$$$$$$$$ ####.$$$$$$ ### "             " ### $$$$$$$$$ s
    '
    |() $$$$$$$$$$$$$ ####.$$$$$ ### "                ####$$$$$$$$s$$'
    |
    ) (($$"$$$$$$$$$$$#####.$$$$$###' -Tua Xiong     .###$$$$$$$$$$"
      | ()) _, $"   $$$$$$$$$$$$######.$$##'                .###$$$$$$$$$$
      |) ((\."$$$$$$$$$$$$$#######,,,.          ..####$$$$$$$$$$$"
      | () $) ), $$$$$$$$$$$$$$$$$$ #################### $$$$$$$$$$$"
      | (($$ (\ _sS"  `" $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$S$$,
      |)
    ) $$$s
    ) )..`$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"'  ` $$
      |($$$Ss /.$
    ,.$
    ,, s$$$$$$ ## S$$$$$$$$$$$$$$$$$$$$$$$$S""
    '
    | \
    ) _$$$$$$$$$$$$$$$$$$$$$$$ ## "  $$        `$$.        `$$.
    | `"S$$$$$$$$$$$$$$$$$#"      $          ` $
    ` $
    |
    ` \
    "\"\"\"\"\"\"\"\"\"\"\""
    ' ' ' '
        |""".stripMargin)
  }

}


