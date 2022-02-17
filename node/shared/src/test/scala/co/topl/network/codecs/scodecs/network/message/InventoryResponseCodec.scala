package co.topl.network.codecs.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.codecs.scodecs.Generators
import co.topl.network.message.Messages.MessagesV1.InventoryResponse
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.message.InventoryResponseSerializer

class InventoryResponseCodec extends CodecCompatabilityBehavior with CommonGenerators {

  val inventoryResponseGen: Gen[InventoryResponse] =
    Gen
      .zip(Generators.modifierTypeIdGen, Gen.nonEmptyListOf(modifierIdGen))
      .map(values => InventoryResponse(values._1, values._2))

  codecCompatabilityBehavior(
    "inventory response",
    inventoryResponseCodec,
    new InventoryResponseSerializer(10000),
    inventoryResponseGen
  )
}
