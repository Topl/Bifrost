package bifrost

import examples.bifrost.transaction.{BifrostBox, BifrostBoxSerializer}
import org.scalacheck.Gen
import scorex.testkit.CoreGenerators

/**
  * Created by cykoz on 4/12/17.
  */
trait BifrostGenerators extends CoreGenerators {
  lazy val stringGen = nonEmptyBytesGen.map(new String(_))

  lazy val bifrostBoxGen: Gen[BifrostBox[String]] = for {
    proposition <- propositionGen
    nonce <- positiveLongGen
    value <- stringGen
  } yield BifrostBox[String](proposition, nonce, value)(new BifrostBoxSerializer[String])
}
