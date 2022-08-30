package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses._
import co.topl.models.{BlockHeaderV2, SpendingAddress, StakingAddress, Transaction}

trait TetraSignableCodecs {

  import TetraImmutableCodecs._
  import co.topl.codecs.bytes.typeclasses.implicits._

  implicit val signableUnsignedBlockHeaderV2: Signable[BlockHeaderV2.Unsigned] =
    _.immutableBytes

  implicit val signableBlockHeaderV2: Signable[BlockHeaderV2] =
    t =>
      BlockHeaderV2
        .Unsigned(
          t.parentHeaderId,
          t.parentSlot,
          t.txRoot,
          t.bloomFilter,
          t.timestamp,
          t.height,
          t.slot,
          t.eligibilityCertificate,
          BlockHeaderV2.Unsigned.PartialOperationalCertificate(
            t.operationalCertificate.parentVK,
            t.operationalCertificate.parentSignature,
            t.operationalCertificate.childVK
          ),
          t.metadata,
          t.address
        )
        .signableBytes

  implicit val signableUnprovenTransaction: Signable[Transaction.Unproven] =
    _.immutableBytes

  implicit val signableTransaction: Signable[Transaction] =
    t =>
      Transaction
        .Unproven(
          t.inputs.map(i => Transaction.Unproven.Input(i.boxId, i.proposition, i.value)),
          t.outputs,
          t.schedule,
          t.data
        )
        .signableBytes

  implicit val signableAddressCommitment: Signable[(SpendingAddress, StakingAddress)] = {
    case (spendingAddress, stakingAddress) =>
      spendingAddress.immutableBytes ++ stakingAddress.immutableBytes
  }
}

object TetraSignableCodecs extends TetraSignableCodecs
