package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses._
import co.topl.{models => legacyModels}
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.crypto.models.VerificationKeyEd25519
import co.topl.models.utility._
import com.google.protobuf.ByteString

trait TetraSignableCodecs {

  import TetraImmutableCodecs._
  import co.topl.codecs.bytes.typeclasses.implicits._

  implicit val signableUnsignedConsensusBlockHeader: Signable[legacyModels.BlockHeader.UnsignedConsensus] =
    _.immutableBytes

  implicit val signableUnsignedBlockHeader: Signable[legacyModels.BlockHeader.Unsigned] =
    t =>
      legacyModels.BlockHeader
        .UnsignedConsensus(
          BlockId(t.parentHeaderId.dataBytes),
          t.parentSlot,
          t.txRoot.data,
          t.bloomFilter.data,
          t.timestamp,
          t.height,
          t.slot,
          ReplaceModelUtil.eligibilityCertificate(t.eligibilityCertificate),
          legacyModels.BlockHeader.UnsignedConsensus.PartialOperationalCertificate(
            ReplaceModelUtil.verificationKeyKesProduct(t.partialOperationalCertificate.parentVK),
            ReplaceModelUtil.signatureKesProduct(t.partialOperationalCertificate.parentSignature),
            VerificationKeyEd25519(t.partialOperationalCertificate.childVK.bytes.data)
          ),
          t.metadata.fold(ByteString.EMPTY)(m => ByteString.copyFrom(m.data.bytes)),
          t.address.vk.bytes.data
        )
        .signableBytes

  implicit val signableBlockConsensusHeader: Signable[BlockHeader] =
    t =>
      legacyModels.BlockHeader
        .UnsignedConsensus(
          t.parentHeaderId.get,
          t.parentSlot,
          t.txRoot,
          t.bloomFilter,
          t.timestamp,
          t.height,
          t.slot,
          t.eligibilityCertificate.get,
          legacyModels.BlockHeader.UnsignedConsensus.PartialOperationalCertificate(
            t.operationalCertificate.get.parentVK,
            t.operationalCertificate.get.parentSignature,
            t.operationalCertificate.get.childVK
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
