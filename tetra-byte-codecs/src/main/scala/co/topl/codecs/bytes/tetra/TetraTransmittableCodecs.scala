package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.{
  BlockHeaderV2,
  FullAddress,
  Proof,
  Proofs,
  Proposition,
  SlotData,
  SpendingAddress,
  Transaction,
  TypedBytes,
  TypedIdentifier
}
import co.topl.models.utility.Ratio

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedBytes] = Transmittable.instanceFromCodec
  implicit val typedIdentifierListTransmittable: Transmittable[List[TypedBytes]] = Transmittable.instanceFromCodec
  implicit val blockHeaderV2Transmittable: Transmittable[BlockHeaderV2] = Transmittable.instanceFromCodec
  implicit val slotDataTransmittable: Transmittable[SlotData] = Transmittable.instanceFromCodec
  implicit val transactionTransmittable: Transmittable[Transaction] = Transmittable.instanceFromCodec
  implicit val propositionTransmittable: Transmittable[Proposition] = Transmittable.instanceFromCodec
  implicit val proofTransmittable: Transmittable[Proof] = Transmittable.instanceFromCodec
  implicit val spendingAddressTransmittable: Transmittable[SpendingAddress] = Transmittable.instanceFromCodec
  implicit val fullAddressTransmittable: Transmittable[FullAddress] = Transmittable.instanceFromCodec

  implicit val kesProductProofTransmittable: Transmittable[Proofs.Knowledge.KesProduct] =
    Transmittable.instanceFromCodec

  implicit val longTypedIdentifierOptTransmittable: Transmittable[(Long, Option[TypedIdentifier])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[TypedIdentifier])
        .as[(Long, Option[TypedIdentifier])]
    )
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
