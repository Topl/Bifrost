package co.topl.nodeCodecs.binary.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.message.Messages.MessagesV1.InventoryResponse
import co.topl.nodeCodecs.binary.legacy.network.message.InventoryResponseSerializer
import co.topl.nodeCodecs.binary.scodecs.Generators
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import co.topl.nodeCatsInstances._

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
