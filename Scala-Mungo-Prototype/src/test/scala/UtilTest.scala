import compilerPlugin.Util._
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

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
}
