package co.topl.codecs.bytes.tetra

import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.utility.Ratio
import co.topl.models._
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader}
import co.topl.node.models.{BlockBody => NodeBlockBody}
import scala.collection.immutable.ListSet

trait TetraTransmittableCodecs {

  import TetraScodecCodecs._
  import co.topl.codecs.bytes.scodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
  implicit val typedIdentifierTransmittable: Transmittable[TypedBytes] = Transmittable.instanceFromCodec
  implicit val typedIdentifierListSetTransmittable: Transmittable[ListSet[TypedBytes]] = Transmittable.instanceFromCodec
  implicit val NodeBlockBodyTransmittable: Transmittable[NodeBlockBody] = Transmittable.instanceFromCodec
  implicit val blockHeaderTransmittable: Transmittable[BlockHeader] = Transmittable.instanceFromCodec
  implicit val consensusBlockHeaderTransmittable: Transmittable[ConsensusBlockHeader] = Transmittable.instanceFromCodec
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
