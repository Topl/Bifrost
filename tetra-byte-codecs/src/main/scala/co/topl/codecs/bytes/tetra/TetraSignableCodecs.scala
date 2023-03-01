package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses._
import co.topl.{models => legacyModels}
import co.topl.consensus.models.BlockHeader
import co.topl.models.utility._

trait TetraSignableCodecs {

  import TetraImmutableCodecs._
  import co.topl.codecs.bytes.typeclasses.implicits._

  implicit val signableUnsignedConsensusBlockHeader: Signable[legacyModels.BlockHeader.Unsigned] =
    _.immutableBytes

  implicit val signableBlockConsensusHeader: Signable[BlockHeader] =
    t =>
      legacyModels.BlockHeader
        .Unsigned(
          t.parentHeaderId,
          t.parentSlot,
          t.txRoot,
          t.bloomFilter,
          t.timestamp,
          t.height,
          t.slot,
          t.eligibilityCertificate,
          legacyModels.BlockHeader.Unsigned.PartialOperationalCertificate(
            t.operationalCertificate.parentVK,
            t.operationalCertificate.parentSignature,
            t.operationalCertificate.childVK
          ),
          t.metadata,
          t.address
        )
        .signableBytes

  implicit val signableBlockHeader: Signable[legacyModels.BlockHeader] =
    t => ReplaceModelUtil.consensusHeader(t).signableBytes

  implicit val signableUnprovenTransaction: Signable[legacyModels.Transaction.Unproven] =
    _.immutableBytes

  implicit val signableTransaction: Signable[legacyModels.Transaction] =
    t =>
      legacyModels.Transaction
        .Unproven(
          t.inputs.map(i => legacyModels.Transaction.Unproven.Input(i.boxId, i.proposition, i.value)),
          t.outputs,
          t.schedule,
          t.data
        )
        .signableBytes

  implicit val signableAddressCommitment: Signable[(legacyModels.SpendingAddress, legacyModels.StakingAddress)] = {
    case (spendingAddress, stakingAddress) =>
      spendingAddress.immutableBytes ++ stakingAddress.immutableBytes
  }
}

object TetraSignableCodecs extends TetraSignableCodecs
