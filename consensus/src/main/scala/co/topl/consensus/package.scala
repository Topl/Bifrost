package co.topl

import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.SlotData
import co.topl.consensus.models.SlotId
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.Bytes
import co.topl.models.UnsignedBlockHeader
import co.topl.models.utility._
import com.google.protobuf.ByteString

package object consensus {

  /**
   * TODO
   * @param blockHeader helper for SlotData
   */
  implicit class ConsensusBlockHeaderOps(blockHeader: BlockHeader) {

    def slotData(implicit ed25519VRF: Ed25519VRF): SlotData =
      SlotData(
        SlotId(blockHeader.slot, blockHeader.id),
        SlotId(blockHeader.parentSlot, blockHeader.parentHeaderId),
        ByteString.copyFrom(ed25519VRF.proofToHash(blockHeader.eligibilityCertificate.vrfSig.toByteArray)),
        blockHeader.eligibilityCertificate.eta,
        blockHeader.height
      )

    def unsigned: UnsignedBlockHeader =
      UnsignedBlockHeader(
        blockHeader.parentHeaderId,
        blockHeader.parentSlot,
        blockHeader.txRoot,
        blockHeader.bloomFilter,
        blockHeader.timestamp,
        blockHeader.height,
        blockHeader.slot,
        blockHeader.eligibilityCertificate,
        UnsignedBlockHeader.PartialOperationalCertificate(
          blockHeader.operationalCertificate.parentVK,
          blockHeader.operationalCertificate.parentSignature,
          blockHeader.operationalCertificate.childVK
        ),
        blockHeader.metadata,
        blockHeader.address
      )
  }

  private val TestStringByteVector = ByteString.copyFromUtf8("TEST")

  private val NonceStringByteVector = ByteString.copyFromUtf8("NONCE")

  def thresholdEvidence(threshold: Ratio)(implicit blake2b256: Blake2b256): ByteString =
    blake2b256.hash(
      threshold.numerator.toByteArray ++ threshold.denominator.toByteArray
    )

  /**
   * @param rho length = 64
   * @return length = 64
   */
  def rhoToRhoTestHash(rho: Bytes)(implicit blake2b512: Blake2b512): Bytes =
    blake2b512.hash(rho.concat(TestStringByteVector))

  /**
   * @param rho length = 64
   * @return length = 64
   */
  def rhoToRhoNonceHash(rho: Bytes)(implicit blake2b512: Blake2b512): Bytes =
    blake2b512.hash(rho.concat(NonceStringByteVector))
}
