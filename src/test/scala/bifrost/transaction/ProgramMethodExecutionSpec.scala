package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks, ScalaCheckDrivenPropertyChecks}

class ProgramMethodExecutionSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

}
