package co.topl.consensus.models

import cats.Monad
import cats.implicits._
import cats.data.EitherT
import co.topl.models.utility.{IsomorphicValueOps, Isomorphism}
import co.topl.consensus.{models => consensusModels}
import co.topl.crypto.{models => cryptoModels}

trait CryptoConsensusMorphismInstances {

  implicit def verificationKeyEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.VerificationKeyEd25519, cryptoModels.VerificationKeyEd25519] =
    Isomorphism(
      _.map(ed2551 => EitherT.pure[F, String](cryptoModels.VerificationKeyEd25519(ed2551.value))).flatMap(_.value),
      _.map(ed2551 => EitherT.pure[F, String](consensusModels.VerificationKeyEd25519(ed2551.value))).flatMap(_.value)
    )

  implicit def verificationKeyKesProductIsomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.VerificationKeyKesProduct, cryptoModels.VerificationKeyKesProduct] =
    Isomorphism(
      _.map(kesProduct =>
        EitherT.pure[F, String](cryptoModels.VerificationKeyKesProduct(kesProduct.value, kesProduct.step))
      ).flatMap(_.value),
      _.map(kesProduct =>
        EitherT.pure[F, String](consensusModels.VerificationKeyKesProduct(kesProduct.value, kesProduct.step))
      ).flatMap(_.value)
    )

  implicit def signatureEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.SignatureEd25519, cryptoModels.SignatureEd25519] =
    Isomorphism(
      _.map(ed2551 => EitherT.pure[F, String](cryptoModels.SignatureEd25519(ed2551.value))).flatMap(_.value),
      _.map(ed2551 => EitherT.pure[F, String](consensusModels.SignatureEd25519(ed2551.value))).flatMap(_.value)
    )

  implicit def signatureKesSumIsomorphism[F[_]: Monad]
    : Isomorphism[F, consensusModels.SignatureKesSum, cryptoModels.SignatureKesSum] =
    Isomorphism[F, consensusModels.SignatureKesSum, cryptoModels.SignatureKesSum](
      _.map(kesSum =>
        for {
          verificationKey <- EitherT(kesSum.verificationKey.toF[F, cryptoModels.VerificationKeyEd25519])
          signature       <- EitherT(kesSum.signature.toF[F, cryptoModels.SignatureEd25519])
          witness         <- EitherT.pure[F, String](kesSum.witness)
        } yield cryptoModels.SignatureKesSum(verificationKey, signature, witness)
      ).flatMap(_.value),
      _.map(kesSum =>
        for {
          verificationKey <- EitherT(kesSum.verificationKey.toF[F, consensusModels.VerificationKeyEd25519])
          signature       <- EitherT(kesSum.signature.toF[F, consensusModels.SignatureEd25519])
          witness         <- EitherT.pure[F, String](kesSum.witness)
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
          subRoot        <- EitherT.pure[F, String](kesProduct.subRoot)
        } yield cryptoModels.SignatureKesProduct(superSignature, subSignature, subRoot)
      ).flatMap(_.value),
      _.map(kesProduct =>
        for {
          superSignature <- EitherT(kesProduct.superSignature.toF[F, consensusModels.SignatureKesSum])
          subSignature   <- EitherT(kesProduct.subSignature.toF[F, consensusModels.SignatureKesSum])
          subRoot        <- EitherT.pure[F, String](kesProduct.subRoot)
        } yield consensusModels.SignatureKesProduct(superSignature, subSignature, subRoot)
      ).flatMap(_.value)
    )
}

object CryptoConsensusMorphismInstances extends CryptoConsensusMorphismInstances
