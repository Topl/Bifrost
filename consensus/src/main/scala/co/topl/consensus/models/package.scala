package co.topl.consensus

import co.topl.consensus.{models => consensusModels}
import co.topl.crypto.{models => cryptoModels}
import com.google.protobuf.ByteString

import scala.language.implicitConversions

package object models {

  implicit def consensusToCryptoVerificationKeyKesProduct(
    kesProduct: consensusModels.VerificationKeyKesProduct
  ): cryptoModels.VerificationKeyKesProduct =
    cryptoModels.VerificationKeyKesProduct(kesProduct.value.toByteArray, kesProduct.step)

  implicit def cryptoToConsensusVerificationKeyKesProduct(
    kesProduct: cryptoModels.VerificationKeyKesProduct
  ): consensusModels.VerificationKeyKesProduct =
    consensusModels.VerificationKeyKesProduct(ByteString.copyFrom(kesProduct.value), kesProduct.step)

  implicit def consensusToCryptoSignatureKesSum(kesSum: consensusModels.SignatureKesSum): cryptoModels.SignatureKesSum =
    cryptoModels.SignatureKesSum(
      kesSum.verificationKey.toByteArray,
      kesSum.signature.toByteArray,
      kesSum.witness.map(_.toByteArray)
    )

  implicit def cryptoToConsensusVerificationKeyKesSum(
    kesSum: cryptoModels.SignatureKesSum
  ): consensusModels.SignatureKesSum =
    consensusModels.SignatureKesSum(
      ByteString.copyFrom(kesSum.verificationKey),
      ByteString.copyFrom(kesSum.signature),
      kesSum.witness.map(ByteString.copyFrom)
    )

  implicit def consensusToCryptoSignatureKesProduct(
    kesProduct: consensusModels.SignatureKesProduct
  ): cryptoModels.SignatureKesProduct =
    cryptoModels.SignatureKesProduct(
      kesProduct.superSignature,
      kesProduct.subSignature,
      kesProduct.subRoot.toByteArray
    )

  implicit def cryptoToConsensusVerificationKeyKesProduct(
    kesProduct: cryptoModels.SignatureKesProduct
  ): consensusModels.SignatureKesProduct =
    consensusModels.SignatureKesProduct(
      kesProduct.superSignature,
      kesProduct.subSignature,
      ByteString.copyFrom(kesProduct.subRoot)
    )
}
