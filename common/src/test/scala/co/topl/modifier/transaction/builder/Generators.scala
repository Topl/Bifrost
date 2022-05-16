package co.topl.modifier.transaction.builder

import cats.data.{Chain, NonEmptyChain}
import co.topl.modifier.box.{ArbitBox, PolyBox}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen

trait Generators {

  private object CommonGenerators extends CommonGenerators

  implicit val polyBoxesGen: Gen[NonEmptyChain[PolyBox]] =
    Gen
      .zip(CommonGenerators.polyBoxGen, Gen.listOf(CommonGenerators.polyBoxGen))
      .map(boxes => NonEmptyChain.one(boxes._1).appendChain(Chain.fromSeq(boxes._2)))

  implicit val arbitBoxesGen: Gen[NonEmptyChain[ArbitBox]] =
    Gen
      .zip(CommonGenerators.arbitBoxGen, Gen.listOf(CommonGenerators.arbitBoxGen))
      .map(boxes => NonEmptyChain.one(boxes._1).appendChain(Chain.fromSeq(boxes._2)))
}

object Generators extends Generators
