package bifrost.transaction

/** Created by cykoz on 5/11/2017.
  */
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class ProgramMethodExecutionSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with BifrostGenerators
    with ValidGenerators {}
