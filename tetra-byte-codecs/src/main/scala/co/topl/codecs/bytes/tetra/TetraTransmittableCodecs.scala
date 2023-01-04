package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.utility.Ratio
import co.topl.models._

import scala.collection.immutable.ListSet

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedBytes] = Transmittable.instanceFromCodec
  implicit val typedIdentifierListSetTransmittable: Transmittable[ListSet[TypedBytes]] = Transmittable.instanceFromCodec
  implicit val blockHeaderTransmittable: Transmittable[BlockHeader] = Transmittable.instanceFromCodec
  implicit val slotDataTransmittable: Transmittable[SlotData] = Transmittable.instanceFromCodec
  implicit val transactionTransmittable: Transmittable[Transaction] = Transmittable.instanceFromCodec

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
