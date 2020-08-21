import org.scalatest.{BeforeAndAfter, FlatSpec, FunSuite, Matchers}
import compilerPlugin.{ClassInfo, GetFileFromAnnotation}

import scala.tools.nsc
import scala.tools.nsc.{Global, Settings, reporters}
import scala.tools.nsc.reporters.Reporter


class PluginUnitTest extends FunSuite with BeforeAndAfter{
  var plugin:GetFileFromAnnotation = _
  var ci:ClassInfo =_

  before{
    plugin = new GetFileFromAnnotation(nsc.Global(new Settings(), reporters.Reporter(new Settings())))

  }

  test("keepmethodname"){

    assert(true)
  }
}
