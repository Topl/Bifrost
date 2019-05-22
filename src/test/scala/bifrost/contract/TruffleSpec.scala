package bifrost.contract

import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import org.graalvm.polyglot._

class TruffleSpec extends PropSpec
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val script =
    """
      |var x = 1;
      |var y = "test";
    """.stripMargin

  val context: Context = Context.create("js", script)


}
