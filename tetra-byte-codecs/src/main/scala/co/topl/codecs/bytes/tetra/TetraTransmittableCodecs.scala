package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.{BlockHeaderV2, SlotData, Transaction, TypedBytes, TypedIdentifier}
import co.topl.models.utility.Ratio

import scala.collection.immutable.ListSet

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedBytes] = Transmittable.instanceFromCodec
  implicit val typedIdentifierListSetTransmittable: Transmittable[ListSet[TypedBytes]] = Transmittable.instanceFromCodec
  implicit val blockHeaderV2Transmittable: Transmittable[BlockHeaderV2] = Transmittable.instanceFromCodec
  implicit val slotDataTransmittable: Transmittable[SlotData] = Transmittable.instanceFromCodec
  implicit val transactionTransmittable: Transmittable[Transaction] = Transmittable.instanceFromCodec

  implicit val longTypedIdentifierOptTransmittable: Transmittable[(Long, Option[TypedIdentifier])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[TypedIdentifier])
        .as[(Long, Option[TypedIdentifier])]
    )
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
