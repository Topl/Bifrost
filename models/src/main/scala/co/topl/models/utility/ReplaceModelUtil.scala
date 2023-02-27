package co.topl.models.utility

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
          .of(typedIdentifier.dataBytes)
      )
    )

  def nodeBlock(blockBody: co.topl.models.BlockBody): co.topl.node.models.BlockBody =
    co.topl.node.models.BlockBody(
      transactionIds = blockBody.toSeq.map(ioTransaction32)
    )

  def consensusHeader(header: co.topl.models.BlockHeader): co.topl.consensus.models.BlockHeader =
    co.topl.consensus.models.BlockHeader(
      parentHeaderId = co.topl.consensus.models.BlockId(header.parentHeaderId.dataBytes),
      parentSlot = header.parentSlot,
      txRoot = header.txRoot.data,
      bloomFilter = header.bloomFilter.data,
      timestamp = header.timestamp,
      height = header.height,
      slot = header.slot,
      eligibilityCertificate = ReplaceModelUtil.eligibilityCertificate(header.eligibilityCertificate),
      operationalCertificate = ReplaceModelUtil.operationalCertificate(header.operationalCertificate),
      metadata = ByteString.copyFrom(header.metadata.map(_.data.bytes).getOrElse(Array.empty)),
      address = header.address.vk.bytes.data
    )

  def operationalCertificate(
    operationalCertificate: co.topl.models.OperationalCertificate
  ): co.topl.consensus.models.OperationalCertificate =
    co.topl.consensus.models.OperationalCertificate(
      parentVK = verificationKeyKesProduct(operationalCertificate.parentVK),
      parentSignature = signatureKesProduct(operationalCertificate.parentSignature),
      childVK = co.topl.consensus.models.VerificationKeyEd25519(
        value = operationalCertificate.childVK.bytes.data
      ),
      childSignature = co.topl.consensus.models.SignatureEd25519(
        value = operationalCertificate.childSignature.bytes.data
      )
    )

  def eligibilityCertificate(
    eligibilityCertificate: co.topl.models.EligibilityCertificate
  ): co.topl.consensus.models.EligibilityCertificate =
    co.topl.consensus.models.EligibilityCertificate(
      vrfSig = co.topl.consensus.models.SignatureVrfEd25519(
        value = eligibilityCertificate.vrfSig.bytes.data
      ),
      vrfVK = co.topl.consensus.models.VerificationKeyVrfEd25519(
        value = eligibilityCertificate.vkVRF.bytes.data
      ),
      thresholdEvidence = eligibilityCertificate.thresholdEvidence.data,
      eta = eligibilityCertificate.eta.data
    )

  def verificationKeyKesProduct(
    vk: co.topl.models.VerificationKeys.KesProduct
  ): co.topl.consensus.models.VerificationKeyKesProduct =
    co.topl.consensus.models.VerificationKeyKesProduct(
      value = vk.bytes.data,
      step = vk.step
    )

  def signatureKesProduct(
    kesProduct: co.topl.models.Proofs.Knowledge.KesProduct
  ): co.topl.consensus.models.SignatureKesProduct =
    co.topl.consensus.models.SignatureKesProduct(
      superSignature = co.topl.consensus.models.SignatureKesSum(
        verificationKey = co.topl.consensus.models.VerificationKeyEd25519(
          kesProduct.superSignature.verificationKey.bytes.data
        ),
        signature = co.topl.consensus.models.SignatureEd25519(
          kesProduct.superSignature.signature.bytes.data
        ),
        witness = kesProduct.superSignature.witness.map(_.data)
      ),
      subSignature = co.topl.consensus.models.SignatureKesSum(
        verificationKey = co.topl.consensus.models.VerificationKeyEd25519(
          kesProduct.subSignature.verificationKey.bytes.data
        ),
        signature = co.topl.consensus.models.SignatureEd25519(
          kesProduct.subSignature.signature.bytes.data
        ),
        witness = kesProduct.subSignature.witness.map(_.data)
      ),
      subRoot = kesProduct.subRoot.data
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
