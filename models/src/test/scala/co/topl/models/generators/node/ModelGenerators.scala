package co.topl.models.generators.node

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

}
object ModelGenerators extends ModelGenerators
