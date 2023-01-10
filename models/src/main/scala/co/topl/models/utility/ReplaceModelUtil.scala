package co.topl.models.utility



/**
 * Delete this file when replacement job is done
 */
object ReplaceModelUtil {

//  def operationalCertificate(operationalCertificate: co.topl.models.OperationalCertificate):
//    co.topl.consensus.models.OperationalCertificate = {
//
//  }

  def signatureKesProduct(kesProduct:  co.topl.models.Proofs.Knowledge.KesProduct):
  co.topl.consensus.models.SignatureKesProduct = {
    co.topl.consensus.models.SignatureKesProduct(
      superSignature = Option(
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
          witness = kesProduct.superSignature.witness.map(w =>
            com.google.protobuf.ByteString.copyFrom(w.data.toArray)
          ),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      subSignature = Option(
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
          witness = kesProduct.subSignature.witness.map(w =>
            com.google.protobuf.ByteString.copyFrom(w.data.toArray)
          ),
          unknownFields = scalapb.UnknownFieldSet.empty
        )
      ),
      subRoot = com.google.protobuf.ByteString.copyFrom(kesProduct.subRoot.data.toArray),
      unknownFields = scalapb.UnknownFieldSet.empty
    )
  }

}
