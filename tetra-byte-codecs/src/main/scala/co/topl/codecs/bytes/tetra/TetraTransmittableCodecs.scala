package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.{BlockHeaderV2, SlotData, Transaction, TypedIdentifier}
import co.topl.models.utility.Ratio

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedIdentifier] = Transmittable.instanceFromCodec
  implicit val typedIdentifierListTransmittable: Transmittable[List[TypedIdentifier]] = Transmittable.instanceFromCodec
  implicit val blockHeaderV2Transmittable: Transmittable[BlockHeaderV2] = Transmittable.instanceFromCodec
  implicit val slotDataTransmittable: Transmittable[SlotData] = Transmittable.instanceFromCodec
  implicit val transactionTransmittable: Transmittable[Transaction] = Transmittable.instanceFromCodec
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
