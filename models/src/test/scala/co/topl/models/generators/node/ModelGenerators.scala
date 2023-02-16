package co.topl.models.generators.node

import cats.implicits.catsSyntaxOptionId
import co.topl.models.generators.brambl.ModelGenerators.arbitraryIoTransaction32
import co.topl.node.models._
import org.scalacheck.{Arbitrary, Gen}

trait ModelGenerators {

  implicit val arbitraryNodeBody: Arbitrary[BlockBody] =
    Arbitrary(
      for {
        ioTx32 <- Gen.listOf(arbitraryIoTransaction32.arbitrary)
      } yield BlockBody.of(ioTx32)
    )

  // TODO remove optionals: https://github.com/Topl/protobuf-specs/pull/37
  implicit val arbitraryBlock: Arbitrary[Block] =
    Arbitrary(
      for {
        header <- co.topl.models.generators.consensus.ModelGenerators.arbitraryHeader.arbitrary
        body   <- co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody.arbitrary
      } yield Block(header.some, body.some)
    )
}
object ModelGenerators extends ModelGenerators
