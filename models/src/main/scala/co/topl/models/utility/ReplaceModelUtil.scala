package co.topl.models.utility

/**
 * Delete this file when replacement job is done
 */
object ReplaceModelUtil {

  def consensusHeader(header: co.topl.models.BlockHeader) = co.topl.consensus.models.BlockHeader(
    parentHeaderId = com.google.protobuf.ByteString.copyFrom(header.parentHeaderId.dataBytes.toArray),
    parentSlot = header.parentSlot,
    txRoot = com.google.protobuf.ByteString.copyFrom(header.txRoot.data.toArray),
    bloomFilter = com.google.protobuf.ByteString.copyFrom(header.bloomFilter.data.toArray),
    timestamp = header.timestamp,
    height = header.height,
    slot = header.slot,
    eligibilityCertificate = Some(ReplaceModelUtil.eligibilityCertificate(header.eligibilityCertificate)),
    operationalCertificate = Some(ReplaceModelUtil.operationalCertificate(header.operationalCertificate)),
    metadata = com.google.protobuf.ByteString.copyFrom(header.metadata.map(_.data.bytes).getOrElse(Array.empty)),
    address = com.google.protobuf.ByteString.copyFrom(header.address.vk.bytes.data.toArray),
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
          value = com.google.protobuf.ByteString.copyFrom(operationalCertificate.childVK.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      childSignature = Some(
        co.topl.crypto.models.SignatureEd25519(
          value = com.google.protobuf.ByteString.copyFrom(operationalCertificate.childSignature.bytes.data.toArray),
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
          value = com.google.protobuf.ByteString.copyFrom(eligibilityCertificate.vrfSig.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      vrfVK = Some(
        co.topl.consensus.models.VerificationKeyVrfEd25519(
          value = com.google.protobuf.ByteString.copyFrom(eligibilityCertificate.vkVRF.bytes.data.toArray),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      thresholdEvidence =
        com.google.protobuf.ByteString.copyFrom(eligibilityCertificate.thresholdEvidence.data.toArray),
      eta = com.google.protobuf.ByteString.copyFrom(eligibilityCertificate.eta.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

  def verificationKeyKesProduct(
    vk: co.topl.models.VerificationKeys.KesProduct
  ): co.topl.consensus.models.VerificationKeyKesProduct =
    co.topl.consensus.models.VerificationKeyKesProduct(
      value = com.google.protobuf.ByteString.copyFrom(vk.bytes.data.toArray),
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
              com.google.protobuf.ByteString
                .copyFrom(kesProduct.superSignature.verificationKey.bytes.data.toArray)
            )
          ),
          signature = Some(
            co.topl.crypto.models.SignatureEd25519(
              com.google.protobuf.ByteString
                .copyFrom(kesProduct.superSignature.signature.bytes.data.toArray)
            )
          ),
          witness = kesProduct.superSignature.witness.map(w => com.google.protobuf.ByteString.copyFrom(w.data.toArray)),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      subSignature = Some(
        co.topl.consensus.models.SignatureKesSum(
          verificationKey = Some(
            co.topl.crypto.models.VerificationKeyEd25519(
              com.google.protobuf.ByteString
                .copyFrom(kesProduct.subSignature.verificationKey.bytes.data.toArray)
            )
          ),
          signature = Some(
            co.topl.crypto.models.SignatureEd25519(
              com.google.protobuf.ByteString
                .copyFrom(kesProduct.subSignature.signature.bytes.data.toArray)
            )
          ),
          witness = kesProduct.subSignature.witness.map(w => com.google.protobuf.ByteString.copyFrom(w.data.toArray)),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      subRoot = com.google.protobuf.ByteString.copyFrom(kesProduct.subRoot.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )

}
