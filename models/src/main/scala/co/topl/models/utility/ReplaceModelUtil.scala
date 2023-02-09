package co.topl.models.utility

import co.topl.models.TypedIdentifier
import com.google.protobuf.ByteString
import co.topl.models.utility._

/**
 * Delete this file when replacement job is done
 */
object ReplaceModelUtil {

  def ioTransaction32(
    typedIdentifier: co.topl.models.TypedIdentifier
  ): co.topl.brambl.models.Identifier.IoTransaction32 =
    co.topl.brambl.models.Identifier.IoTransaction32(
      co.topl.brambl.models.Evidence.Sized32.of(
        quivr.models.Digest.Digest32
          .of(ByteString.copyFrom(typedIdentifier.dataBytes.toArray))
      )
    )

  def nodeBlock(blockBody: co.topl.models.BlockBody): co.topl.node.models.BlockBody =
    co.topl.node.models.BlockBody(
      transactionIds = blockBody.toSeq.map(ioTransaction32)
    )

  def consensusHeader(header: co.topl.models.BlockHeader): co.topl.consensus.models.BlockHeader =
    co.topl.consensus.models.BlockHeader(
      parentHeaderId = co.topl.consensus.models.BlockId(
        ByteString.copyFrom(header.parentHeaderId.dataBytes.toArray)
      ),
      parentSlot = header.parentSlot,
      txRoot = ByteString.copyFrom(header.txRoot.data.toArray),
      bloomFilter = ByteString.copyFrom(header.bloomFilter.data.toArray),
      timestamp = header.timestamp,
      height = header.height,
      slot = header.slot,
      eligibilityCertificate = ReplaceModelUtil.eligibilityCertificate(header.eligibilityCertificate),
      operationalCertificate = ReplaceModelUtil.operationalCertificate(header.operationalCertificate),
      metadata = ByteString.copyFrom(header.metadata.map(_.data.bytes).getOrElse(Array.empty)),
      address = ByteString.copyFrom(header.address.vk.bytes.data.toArray)
    )

  def operationalCertificate(
    operationalCertificate: co.topl.models.OperationalCertificate
  ): co.topl.consensus.models.OperationalCertificate =
    co.topl.consensus.models.OperationalCertificate(
      parentVK = verificationKeyKesProduct(operationalCertificate.parentVK),
      parentSignature = signatureKesProduct(operationalCertificate.parentSignature),
      childVK = co.topl.crypto.models.VerificationKeyEd25519(
        value = ByteString.copyFrom(operationalCertificate.childVK.bytes.data.toArray)
      ),
      childSignature = co.topl.crypto.models.SignatureEd25519(
        value = ByteString.copyFrom(operationalCertificate.childSignature.bytes.data.toArray)
      )
    )

  def eligibilityCertificate(
    eligibilityCertificate: co.topl.models.EligibilityCertificate
  ): co.topl.consensus.models.EligibilityCertificate =
    co.topl.consensus.models.EligibilityCertificate(
      vrfSig = co.topl.consensus.models.SignatureVrfEd25519(
        value = ByteString.copyFrom(eligibilityCertificate.vrfSig.bytes.data.toArray)
      ),
      vrfVK = co.topl.consensus.models.VerificationKeyVrfEd25519(
        value = ByteString.copyFrom(eligibilityCertificate.vkVRF.bytes.data.toArray)
      ),
      thresholdEvidence = ByteString.copyFrom(eligibilityCertificate.thresholdEvidence.data.toArray),
      eta = ByteString.copyFrom(eligibilityCertificate.eta.data.toArray)
    )

  def verificationKeyKesProduct(
    vk: co.topl.models.VerificationKeys.KesProduct
  ): co.topl.consensus.models.VerificationKeyKesProduct =
    co.topl.consensus.models.VerificationKeyKesProduct(
      value = ByteString.copyFrom(vk.bytes.data.toArray),
      step = vk.step
    )

  def signatureKesProduct(
    kesProduct: co.topl.models.Proofs.Knowledge.KesProduct
  ): co.topl.consensus.models.SignatureKesProduct =
    co.topl.consensus.models.SignatureKesProduct(
      superSignature = co.topl.consensus.models.SignatureKesSum(
        verificationKey = co.topl.crypto.models.VerificationKeyEd25519(
          ByteString
            .copyFrom(kesProduct.superSignature.verificationKey.bytes.data.toArray)
        ),
        signature = co.topl.crypto.models.SignatureEd25519(
          ByteString
            .copyFrom(kesProduct.superSignature.signature.bytes.data.toArray)
        ),
        witness = kesProduct.superSignature.witness.map(w => ByteString.copyFrom(w.data.toArray))
      ),
      subSignature = co.topl.consensus.models.SignatureKesSum(
        verificationKey = co.topl.crypto.models.VerificationKeyEd25519(
          ByteString
            .copyFrom(kesProduct.subSignature.verificationKey.bytes.data.toArray)
        ),
        signature = co.topl.crypto.models.SignatureEd25519(
          ByteString
            .copyFrom(kesProduct.subSignature.signature.bytes.data.toArray)
        ),
        witness = kesProduct.subSignature.witness.map(w => ByteString.copyFrom(w.data.toArray))
      ),
      subRoot = ByteString.copyFrom(kesProduct.subRoot.data.toArray)
    )

  def slotIdToLegacy(slotId: co.topl.consensus.models.SlotId): co.topl.models.SlotId =
    co.topl.models.SlotId(
      slot = slotId.slot,
      blockId = slotId.blockId: TypedIdentifier
    )

  def slotDataFromLegacy(slotDataLegacy: co.topl.models.SlotDataLegacy): co.topl.consensus.models.SlotData =
    co.topl.consensus.models.SlotData.of(
      slotId = co.topl.consensus.models.SlotId.of(
        slot = slotDataLegacy.slotId.slot,
        blockId = co.topl.consensus.models.BlockId.of(
          value = slotDataLegacy.slotId.blockId.dataBytes
        )
      ),
      parentSlotId = co.topl.consensus.models.SlotId.of(
        slot = slotDataLegacy.parentSlotId.slot,
        blockId = co.topl.consensus.models.BlockId.of(
          value = slotDataLegacy.parentSlotId.blockId.dataBytes
        )
      ),
      rho = slotDataLegacy.rho.sizedBytes.data,
      height = slotDataLegacy.height,
      eta = slotDataLegacy.eta.data
    )

}
