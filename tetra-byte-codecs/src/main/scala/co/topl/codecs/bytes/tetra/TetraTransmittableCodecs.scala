package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.utility.Ratio
import co.topl.{models => legacyModels}
import legacyModels._
import co.topl.proto.{models => protoModels}
import co.topl.consensus.{models => consensusModels}
import co.topl.node.{models => nodeModels}
import scala.collection.immutable.ListSet

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedBytes] = Transmittable.instanceFromCodec
  implicit val typedIdentifierListSetTransmittable: Transmittable[ListSet[TypedBytes]] = Transmittable.instanceFromCodec
  implicit val nodeBlockBodyTransmittable: Transmittable[nodeModels.BlockBody] = Transmittable.instanceFromCodec
  implicit val blockHeaderTransmittable: Transmittable[BlockHeader] = Transmittable.instanceFromCodec

  implicit val consensusBlockHeaderTransmittable: Transmittable[consensusModels.BlockHeader] =
    Transmittable.instanceFromCodec
  implicit val slotDataTransmittable: Transmittable[SlotDataLegacy] = Transmittable.instanceFromCodec
  implicit val consensusSlotDataTransmittable: Transmittable[consensusModels.SlotData] = Transmittable.instanceFromCodec
  implicit val transactionTransmittable: Transmittable[Transaction] = Transmittable.instanceFromCodec
  implicit val transactionProtoTransmittable: Transmittable[protoModels.Transaction] = Transmittable.instanceFromCodec

  implicit val eligibilityCertificateTransmittable: Transmittable[EligibilityCertificate] =
    Transmittable.instanceFromCodec

  implicit val operationalCertificateTransmittable: Transmittable[OperationalCertificate] =
    Transmittable.instanceFromCodec

  implicit val operatorStakingAddressTransmittable: Transmittable[StakingAddresses.Operator] =
    Transmittable.instanceFromCodec

  implicit val longTypedIdentifierOptTransmittable: Transmittable[(Long, Option[TypedIdentifier])] =
    Transmittable.instanceFromCodec(
      (longCodec :: optionCodec[TypedIdentifier])
        .as[(Long, Option[TypedIdentifier])]
    )
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
