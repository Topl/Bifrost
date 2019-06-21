package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class ProgramMethodExecutionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

}
