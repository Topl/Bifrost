package co.topl.consensus.models

import cats.Monad
import cats.implicits._
import cats.data.EitherT
import co.topl.models.utility.{IsomorphicValueOps, Isomorphism}
import co.topl.consensus.{models => consensusModels}
import co.topl.crypto.{models => cryptoModels}
import com.google.protobuf.ByteString

trait CryptoConsensusMorphismInstances {

  implicit def verificationKeyKesProductIsomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.VerificationKeyKesProduct, cryptoModels.VerificationKeyKesProduct] =
    Isomorphism(
      _.map(kesProduct =>
        EitherT.pure[F, String](cryptoModels.VerificationKeyKesProduct(kesProduct.value.toByteArray, kesProduct.step))
      ).flatMap(_.value),
      _.map(kesProduct =>
        EitherT.pure[F, String](
          consensusModels.VerificationKeyKesProduct(ByteString.copyFrom(kesProduct.value), kesProduct.step)
        )
      ).flatMap(_.value)
    )

  implicit def signatureKesSumIsomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.SignatureKesSum, cryptoModels.SignatureKesSum] =
    Isomorphism[F, consensusModels.SignatureKesSum, cryptoModels.SignatureKesSum](
      _.map(kesSum =>
        for {
          verificationKey <- EitherT.pure[F, String](kesSum.verificationKey.value.toByteArray)
          signature       <- EitherT.pure[F, String](kesSum.signature.value.toByteArray)
          witness         <- EitherT.pure[F, String](kesSum.witness.map(_.toByteArray))
        } yield cryptoModels.SignatureKesSum(verificationKey, signature, witness)
      ).flatMap(_.value),
      _.map(kesSum =>
        for {
          verificationKey <- EitherT.pure[F, String](
            consensusModels.VerificationKeyEd25519.of(ByteString.copyFrom(kesSum.verificationKey))
          )
          signature <- EitherT.pure[F, String](consensusModels.SignatureEd25519(ByteString.copyFrom(kesSum.signature)))
          witness   <- EitherT.pure[F, String](kesSum.witness.map(ByteString.copyFrom))
        } yield consensusModels.SignatureKesSum(verificationKey, signature, witness)
      ).flatMap(_.value)
    )

  implicit def signatureKesProductIsomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.SignatureKesProduct, cryptoModels.SignatureKesProduct] =
    Isomorphism(
      _.map(kesProduct =>
        for {
          superSignature <- EitherT(kesProduct.superSignature.toF[F, cryptoModels.SignatureKesSum])
          subSignature   <- EitherT(kesProduct.subSignature.toF[F, cryptoModels.SignatureKesSum])
          subRoot        <- EitherT.pure[F, String](kesProduct.subRoot.toByteArray)
        } yield cryptoModels.SignatureKesProduct(superSignature, subSignature, subRoot)
      ).flatMap(_.value),
      _.map(kesProduct =>
        for {
          superSignature <- EitherT(kesProduct.superSignature.toF[F, consensusModels.SignatureKesSum])
          subSignature   <- EitherT(kesProduct.subSignature.toF[F, consensusModels.SignatureKesSum])
          subRoot        <- EitherT.pure[F, String](ByteString.copyFrom(kesProduct.subRoot))
        } yield consensusModels.SignatureKesProduct(superSignature, subSignature, subRoot)
      ).flatMap(_.value)
    )
}

object CryptoConsensusMorphismInstances extends CryptoConsensusMorphismInstances
