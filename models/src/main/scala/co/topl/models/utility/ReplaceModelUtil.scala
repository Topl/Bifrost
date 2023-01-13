package co.topl.models.utility

import com.google.protobuf.ByteString

/**
 * Delete this file when replacement job is done
 */
object ReplaceModelUtil {

  def ioTransaction32(
    typedIdentifier: co.topl.models.TypedIdentifier
  ): co.topl.brambl.models.Identifier.IoTransaction32 =
    co.topl.brambl.models.Identifier.IoTransaction32(
      Some(
        co.topl.brambl.models.Evidence.Sized32.of(
          Some(
            quivr.models.Digest.Digest32
              .of(com.google.protobuf.ByteString.copyFrom(typedIdentifier.dataBytes.toArray))
          )
        )
      )
    )

  def nodeBlock(blockBody: co.topl.models.BlockBody): co.topl.node.models.BlockBody =
    co.topl.node.models.BlockBody(
      transactionIds = blockBody.toSeq.map(ioTransaction32),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

  def consensusHeader(header: co.topl.models.BlockHeader): co.topl.consensus.models.BlockHeader =
    co.topl.consensus.models.BlockHeader(
      parentHeaderId = ByteString.copyFrom(header.parentHeaderId.dataBytes.toArray),
      parentSlot = header.parentSlot,
      txRoot = ByteString.copyFrom(header.txRoot.data.toArray),
      bloomFilter = ByteString.copyFrom(header.bloomFilter.data.toArray),
      timestamp = header.timestamp,
      height = header.height,
      slot = header.slot,
      eligibilityCertificate = Some(ReplaceModelUtil.eligibilityCertificate(header.eligibilityCertificate)),
      operationalCertificate = Some(ReplaceModelUtil.operationalCertificate(header.operationalCertificate)),
      metadata = ByteString.copyFrom(header.metadata.map(_.data.bytes).getOrElse(Array.empty)),
      address = ByteString.copyFrom(header.address.vk.bytes.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

  def operationalCertificate(
    operationalCertificate: co.topl.models.OperationalCertificate
  ): co.topl.consensus.models.OperationalCertificate =
    co.topl.consensus.models.OperationalCertificate(
      parentVK = Some(verificationKeyKesProduct(operationalCertificate.parentVK)),
      parentSignature = Some(signatureKesProduct(operationalCertificate.parentSignature)),
      childVK = Some(
        co.topl.crypto.models.VerificationKeyEd25519(
          value = ByteString.copyFrom(operationalCertificate.childVK.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      childSignature = Some(
        co.topl.crypto.models.SignatureEd25519(
          value = ByteString.copyFrom(operationalCertificate.childSignature.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

  def eligibilityCertificate(
    eligibilityCertificate: co.topl.models.EligibilityCertificate
  ): co.topl.consensus.models.EligibilityCertificate =
    co.topl.consensus.models.EligibilityCertificate(
      vrfSig = Some(
        co.topl.consensus.models.SignatureVrfEd25519(
          value = ByteString.copyFrom(eligibilityCertificate.vrfSig.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      vrfVK = Some(
        co.topl.consensus.models.VerificationKeyVrfEd25519(
          value = ByteString.copyFrom(eligibilityCertificate.vkVRF.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      thresholdEvidence = ByteString.copyFrom(eligibilityCertificate.thresholdEvidence.data.toArray),
      eta = ByteString.copyFrom(eligibilityCertificate.eta.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

  def verificationKeyKesProduct(
    vk: co.topl.models.VerificationKeys.KesProduct
  ): co.topl.consensus.models.VerificationKeyKesProduct =
    co.topl.consensus.models.VerificationKeyKesProduct(
      value = ByteString.copyFrom(vk.bytes.data.toArray),
      step = vk.step,
      unknownFields = scalapb.UnknownFieldSet.empty
    )

  def signatureKesProduct(
    kesProduct: co.topl.models.Proofs.Knowledge.KesProduct
  ): co.topl.consensus.models.SignatureKesProduct =
    co.topl.consensus.models.SignatureKesProduct(
      superSignature = Some(
        co.topl.consensus.models.SignatureKesSum(
          verificationKey = Some(
            co.topl.crypto.models.VerificationKeyEd25519(
              ByteString
                .copyFrom(kesProduct.superSignature.verificationKey.bytes.data.toArray)
            )
          ),
          signature = Some(
            co.topl.crypto.models.SignatureEd25519(
              ByteString
                .copyFrom(kesProduct.superSignature.signature.bytes.data.toArray)
            )
          ),
          witness = kesProduct.superSignature.witness.map(w => ByteString.copyFrom(w.data.toArray)),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      subSignature = Some(
        co.topl.consensus.models.SignatureKesSum(
          verificationKey = Some(
            co.topl.crypto.models.VerificationKeyEd25519(
              ByteString
                .copyFrom(kesProduct.subSignature.verificationKey.bytes.data.toArray)
            )
          ),
          signature = Some(
            co.topl.crypto.models.SignatureEd25519(
              ByteString
                .copyFrom(kesProduct.subSignature.signature.bytes.data.toArray)
            )
          ),
          witness = kesProduct.subSignature.witness.map(w => ByteString.copyFrom(w.data.toArray)),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      subRoot = ByteString.copyFrom(kesProduct.subRoot.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

}
