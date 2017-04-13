package bifrost

import examples.bifrost.transaction.box.ContractBox
import org.scalacheck.Gen
import scorex.testkit.CoreGenerators

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {
  lazy val stringGen = nonEmptyBytesGen.map(new String(_))

  lazy val bifrostBoxGen: Gen[ContractBox] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- stringGen
  } yield ContractBox(proposition, nonce, value, "This is an Agreement")
}
