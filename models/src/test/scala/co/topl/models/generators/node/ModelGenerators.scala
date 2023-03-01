package co.topl.models.generators.node

import co.topl.brambl.generators.ModelGenerators.arbitraryIoTransaction32
import co.topl.models.generators.consensus.ModelGenerators.arbitraryHeader
import co.topl.node.models._
import org.scalacheck.{Arbitrary, Gen}

trait ModelGenerators {

  implicit val arbitraryNodeBody: Arbitrary[BlockBody] =
    Arbitrary(
      for {
        ioTx32 <- Gen.listOf(arbitraryIoTransaction32.arbitrary)
      } yield BlockBody.of(ioTx32)
    )

  implicit val arbitraryBlock: Arbitrary[Block] =
    Arbitrary(
      for {
        header <- arbitraryHeader.arbitrary
        body   <- arbitraryNodeBody.arbitrary
      } yield Block.of(header, body)
    )
}
object ModelGenerators extends ModelGenerators
