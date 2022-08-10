package co.topl.grpc

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{HasLength, Lengths, Sized}
import co.topl.{models => bifrostModels}
import com.google.protobuf.ByteString

import scala.collection.immutable.ListSet

trait BifrostMorphismInstances
    extends PrimitiveBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with ProofBifrostMorphismInstances
    with AddressBifrostMorphismInstances
    with CertificateBifrostMorphismInstances
    with BlockBifrostMorphismInstances

trait PrimitiveBifrostMorphismInstances {

  implicit def bytesIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Bytes, ByteString] =
    Isomorphism(
      _.map(v => (v: ByteString).asRight[String]),
      _.map(v => (v: bifrostModels.Bytes).asRight[String])
    )

  implicit def latin1DataIsomorphism[F[_]: Monad]: Isomorphism[F, Latin1Data, ByteString] =
    Isomorphism(
      _.map(v => ByteString.copyFrom(v.bytes).asRight[String]),
      _.map(v => Latin1Data.fromData(v.toByteArray).asRight[String])
    )

  implicit def fromSizedStrictMorphism[F[_]: Monad, A: HasLength, B, L <: bifrostModels.utility.Length](implicit
    subMorphism: Morphism[F, A, B],
    l:           L
  ): Morphism[F, bifrostModels.utility.Sized.Strict[A, L], B] =
    _.map(_.data).to[B]

  implicit def toSizedStrictMorphism[F[_]: Monad, A, B: HasLength, L <: bifrostModels.utility.Length](implicit
    subMorphism: Morphism[F, A, B],
    l:           L
  ): Morphism[F, A, bifrostModels.utility.Sized.Strict[B, L]] =
    fa =>
      EitherT(fa.to[B])
        .subflatMap(Sized.strict[B, L](_).leftMap(_ => "Invalid length"))
        .value

  implicit def fromSizedMaxMorphism[F[_]: Monad, A: HasLength, B, L <: bifrostModels.utility.Length](implicit
    subMorphism: Morphism[F, A, B],
    l:           L
  ): Morphism[F, bifrostModels.utility.Sized.Max[A, L], B] =
    _.map(_.data).to[B]

  implicit def toSizedMaxMorphism[F[_]: Monad, A, B: HasLength, L <: bifrostModels.utility.Length](implicit
    subMorphism: Morphism[F, A, B],
    l:           L
  ): Morphism[F, A, bifrostModels.utility.Sized.Max[B, L]] =
    fa =>
      EitherT(fa.to[B])
        .subflatMap(Sized.max[B, L](_).leftMap(_ => "Invalid length"))
        .value

}

trait VerificationKeyBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances =>

  implicit def verificationKeysEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.Ed25519, models.VerificationKeyEd25519] =
    Isomorphism(
      _.map(p =>
        for {
          value <- EitherT(p.bytes.toF[F, ByteString])
        } yield models.VerificationKeyEd25519(value)
      ).flatMap(_.value),
      _.map(p =>
        for {
          value <- EitherT(
            p.value.toF[F, Sized.Strict[bifrostModels.Bytes, bifrostModels.VerificationKeys.Ed25519.Length]]
          )
        } yield bifrostModels.VerificationKeys.Ed25519(value)
      ).flatMap(_.value)
    )

  implicit def verificationKeysVrfEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.VrfEd25519, models.VerificationKeyVrfEd25519] =
    Isomorphism(
      _.map(p =>
        for {
          value <- EitherT(p.bytes.toF[F, ByteString])
        } yield models.VerificationKeyVrfEd25519(value)
      ).flatMap(_.value),
      _.map(p =>
        for {
          value <- EitherT(
            p.value.toF[F, Sized.Strict[bifrostModels.Bytes, bifrostModels.VerificationKeys.VrfEd25519.Length]]
          )
        } yield bifrostModels.VerificationKeys.VrfEd25519(value)
      ).flatMap(_.value)
    )

  implicit def verificationKeysKesProductIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.KesProduct, models.VerificationKeyKesProduct] =
    Isomorphism(
      _.map(p =>
        for {
          value <- EitherT(p.bytes.toF[F, ByteString])
          step = p.step
        } yield models.VerificationKeyKesProduct(value, step)
      ).flatMap(_.value),
      _.map(p =>
        for {
          value <- EitherT(
            p.value.toF[F, Sized.Strict[bifrostModels.Bytes, bifrostModels.VerificationKeys.KesProduct.Length]]
          )
          step = p.step
        } yield bifrostModels.VerificationKeys.KesProduct(value, step)
      ).flatMap(_.value)
    )

}

trait CommonBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances =>

  implicit def networkPrefixIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.NetworkPrefix, models.NetworkPrefix] =
    Isomorphism(
      _.map(prefix => models.NetworkPrefix(prefix.value.toInt).asRight[String]),
      _.map(prefix =>
        EitherT.cond[F](
          prefix.value <= Byte.MaxValue,
          bifrostModels.NetworkPrefix(prefix.value.toByte),
          "Invalid networkPrefix"
        )
      )
        .flatMap(_.value)
    )

  implicit def typedEvidenceIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.TypedEvidence, models.TypedEvidence] =
    Isomorphism(
      _.map(p =>
        for {
          prefix   <- p.typePrefix.toInt.asRight[String].toEitherT[F]
          evidence <- EitherT(p.evidence.toF[F, ByteString])
        } yield models.TypedEvidence(prefix, evidence)
      ).flatMap(_.value),
      _.map(p =>
        for {
          prefix   <- EitherT.cond[F](p.typePrefix < Byte.MaxValue, p.typePrefix.toByte, "Invalid typePrefix")
          evidence <- EitherT(p.evidence.toF[F, bifrostModels.Evidence])
        } yield bifrostModels.TypedEvidence(prefix, evidence)
      ).flatMap(_.value)
    )

  implicit def transactionIdIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.TypedIdentifier, models.TransactionId] =
    Isomorphism(
      _.map(v => models.TransactionId(v.dataBytes).asRight[String]),
      _.map(v =>
        Either.cond(
          v.value.length == 32,
          bifrostModels.TypedBytes(bifrostModels.IdentifierTypes.Transaction, v.value),
          "Invalid ID length"
        )
      )
    )

  implicit def blockIdIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.TypedIdentifier, models.BlockId] =
    Isomorphism(
      _.map(v => models.BlockId(v.dataBytes).asRight[String]),
      _.map(v =>
        Either.cond(
          v.value.length == 32,
          bifrostModels.TypedBytes(bifrostModels.IdentifierTypes.Block.HeaderV2, v.value),
          "Invalid ID length"
        )
      )
    )

}

trait ProofBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances =>

  implicit def proofsUndefinedIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Undefined.type, models.ProofUndefined] =
    Isomorphism(
      _.as(models.ProofUndefined().asRight[String]),
      _.as(bifrostModels.Proofs.Undefined.asRight[String])
    )

  implicit def proofsKnowledgeEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.Ed25519, models.ProofKnowledgeEd25519] =
    Isomorphism(
      _.map(p => models.ProofKnowledgeEd25519(p.bytes.data).asRight[String]),
      _.flatMap(p =>
        EitherT(p.value.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`64`.type]])
          .map(bifrostModels.Proofs.Knowledge.Ed25519(_))
          .value
      )
    )

  implicit def proofsKnowledgeVrfEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.VrfEd25519, models.ProofKnowledgeVrfEd25519] =
    Isomorphism(
      _.map(p => models.ProofKnowledgeVrfEd25519(p.bytes.data).asRight[String]),
      _.flatMap(p =>
        EitherT(p.value.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`80`.type]])
          .map(bifrostModels.Proofs.Knowledge.VrfEd25519(_))
          .value
      )
    )

  implicit def proofsKnowledgeKesSumIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.KesSum, models.ProofKnowledgeKesSum] =
    Isomorphism(
      _.map(p =>
        for {
          vk        <- EitherT(p.verificationKey.toF[F, models.VerificationKeyEd25519])
          signature <- EitherT(p.signature.toF[F, models.ProofKnowledgeEd25519])
          witness = p.witness.map(_.data: ByteString)
        } yield models.ProofKnowledgeKesSum(vk.some, signature.some, witness)
      ).flatMap(_.value),
      _.map(p =>
        for {
          vk <- p.verificationKey
            .toRight("Missing verificationKey")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.VerificationKeys.Ed25519])
          signature <- p.signature
            .toRight("Missing signature")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.Ed25519])
          witness <- EitherT(
            p.witness.toVector
              .traverse(_.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`32`.type]])
              .map(_.sequence)
          )
        } yield bifrostModels.Proofs.Knowledge.KesSum(vk, signature, witness)
      ).flatMap(_.value)
    )

  implicit def proofsKnowledgeKesProductIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.KesProduct, models.ProofKnowledgeKesProduct] =
    Isomorphism(
      _.map(p =>
        for {
          superSignature <- EitherT(p.superSignature.toF[F, models.ProofKnowledgeKesSum])
          subSignature   <- EitherT(p.subSignature.toF[F, models.ProofKnowledgeKesSum])
          subRoot        <- EitherT(p.subRoot.toF[F, ByteString])
        } yield models.ProofKnowledgeKesProduct(superSignature.some, subSignature.some, subRoot)
      ).flatMap(_.value),
      _.map(p =>
        for {
          superSignature <- p.superSignature
            .toRight("Missing superSignature")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.KesSum])
          subSignature <- p.subSignature
            .toRight("Missing subSignature")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.KesSum])
          subRoot <- EitherT(p.subRoot.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`32`.type]])
        } yield bifrostModels.Proofs.Knowledge.KesProduct(superSignature, subSignature, subRoot)
      ).flatMap(_.value)
    )

  implicit def proofsKnowledgeHashLockIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.HashLock, models.ProofKnowledgeHashLock] =
    Isomorphism(
      _.map(p =>
        for {
          value <- EitherT(p.value.toF[F, ByteString])
        } yield models.ProofKnowledgeHashLock(value)
      ).flatMap(_.value),
      _.map(p =>
        for {
          value <- EitherT(p.value.toF[F, bifrostModels.Bytes])
        } yield bifrostModels.Proofs.Knowledge.HashLock(value)
      ).flatMap(_.value)
    )

  implicit def proofsCompositionalAndIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Compositional.And, models.ProofCompositionalAnd] =
    Isomorphism(
      _.map(p =>
        for {
          a <- EitherT(p.a.toF[F, models.Proof])
          b <- EitherT(p.b.toF[F, models.Proof])
        } yield models.ProofCompositionalAnd(a, b)
      ).flatMap(_.value),
      _.map(p =>
        for {
          a <- EitherT(p.a.toF[F, bifrostModels.Proof])
          b <- EitherT(p.b.toF[F, bifrostModels.Proof])
        } yield bifrostModels.Proofs.Compositional.And(a, b)
      ).flatMap(_.value)
    )

  implicit def proofsCompositionalOrIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Compositional.Or, models.ProofCompositionalOr] =
    Isomorphism(
      _.map(p =>
        for {
          a <- EitherT(p.a.toF[F, models.Proof])
          b <- EitherT(p.b.toF[F, models.Proof])
        } yield models.ProofCompositionalOr(a, b)
      ).flatMap(_.value),
      _.map(p =>
        for {
          a <- EitherT(p.a.toF[F, bifrostModels.Proof])
          b <- EitherT(p.b.toF[F, bifrostModels.Proof])
        } yield bifrostModels.Proofs.Compositional.Or(a, b)
      ).flatMap(_.value)
    )

  implicit def proofsCompositionalThresholdIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Compositional.Threshold, models.ProofCompositionalThreshold] =
    Isomorphism(
      _.map(p =>
        for {
          proofs <- EitherT(
            p.proofs
              .traverse(_.toF[F, models.Proof])
              .map(_.sequence)
          )
        } yield models.ProofCompositionalThreshold(proofs)
      ).flatMap(_.value),
      _.map(p =>
        for {
          proofs <- EitherT(
            p.proofs.toList
              .traverse(_.toF[F, bifrostModels.Proof])
              .map(_.sequence)
          )
        } yield bifrostModels.Proofs.Compositional.Threshold(proofs)
      ).flatMap(_.value)
    )

  implicit def proofsCompositionalNotIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Compositional.Not, models.ProofCompositionalNot] =
    Isomorphism(
      _.map(p =>
        for {
          a <- EitherT(p.a.toF[F, models.Proof])
        } yield models.ProofCompositionalNot(a)
      ).flatMap(_.value),
      _.map(p =>
        for {
          a <- EitherT(p.a.toF[F, bifrostModels.Proof])
        } yield bifrostModels.Proofs.Compositional.Not(a)
      ).flatMap(_.value)
    )

  implicit def proofsContextualHeightLockIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Contextual.HeightLock, models.ProofContextualHeightLock] =
    Isomorphism(
      _.as(models.ProofContextualHeightLock().asRight[String]),
      _.as(bifrostModels.Proofs.Contextual.HeightLock().asRight[String])
    )

  implicit def proofsContextualRequiredTransactionIOIsomorphism[F[_]: Monad]: Isomorphism[
    F,
    bifrostModels.Proofs.Contextual.RequiredTransactionIO,
    models.ProofContextualRequiredTransactionIO
  ] =
    Isomorphism(
      _.as(models.ProofContextualRequiredTransactionIO().asRight[String]),
      _.as(bifrostModels.Proofs.Contextual.RequiredTransactionIO().asRight[String])
    )

  implicit def proofIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Proof, models.Proof] =
    Isomorphism(
      _.flatMap {
        case bifrostModels.Proofs.Undefined =>
          EitherT(bifrostModels.Proofs.Undefined.toF[F, models.ProofUndefined]).widen[models.Proof].value
        case _: bifrostModels.Proofs.Knowledge.Curve25519 =>
          "Curve25519 Unsupported".asLeft[models.Proof].pure[F]
        case p: bifrostModels.Proofs.Knowledge.Ed25519 =>
          EitherT(p.toF[F, models.ProofKnowledgeEd25519]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Knowledge.VrfEd25519 =>
          EitherT(p.toF[F, models.ProofKnowledgeVrfEd25519]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Knowledge.KesSum =>
          EitherT(p.toF[F, models.ProofKnowledgeKesSum]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Knowledge.KesProduct =>
          EitherT(p.toF[F, models.ProofKnowledgeKesProduct]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Knowledge.HashLock =>
          EitherT(p.toF[F, models.ProofKnowledgeHashLock]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Compositional.And =>
          EitherT(p.toF[F, models.ProofCompositionalAnd]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Compositional.Or =>
          EitherT(p.toF[F, models.ProofCompositionalOr]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Compositional.Threshold =>
          EitherT(p.toF[F, models.ProofCompositionalThreshold]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Compositional.Not =>
          EitherT(p.toF[F, models.ProofCompositionalNot]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Contextual.HeightLock =>
          EitherT(p.toF[F, models.ProofContextualHeightLock]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Contextual.RequiredTransactionIO =>
          EitherT(p.toF[F, models.ProofContextualRequiredTransactionIO]).widen[models.Proof].value
      },
      _.flatMap {
        case models.Proof.Empty =>
          "Empty proof".asLeft[bifrostModels.Proof].toEitherT[F].value
        case p: models.ProofUndefined =>
          EitherT(p.toF[F, bifrostModels.Proofs.Undefined.type]).widen[bifrostModels.Proof].value
        case p: models.ProofKnowledgeEd25519 =>
          EitherT(p.toF[F, bifrostModels.Proofs.Knowledge.Ed25519]).widen[bifrostModels.Proof].value
        case p: models.ProofKnowledgeVrfEd25519 =>
          EitherT(p.toF[F, bifrostModels.Proofs.Knowledge.VrfEd25519]).widen[bifrostModels.Proof].value
        case p: models.ProofKnowledgeKesSum =>
          EitherT(p.toF[F, bifrostModels.Proofs.Knowledge.KesSum]).widen[bifrostModels.Proof].value
        case p: models.ProofKnowledgeKesProduct =>
          EitherT(p.toF[F, bifrostModels.Proofs.Knowledge.KesProduct]).widen[bifrostModels.Proof].value
        case p: models.ProofKnowledgeHashLock =>
          EitherT(p.toF[F, bifrostModels.Proofs.Knowledge.HashLock]).widen[bifrostModels.Proof].value
        case p: models.ProofCompositionalAnd =>
          EitherT(p.toF[F, bifrostModels.Proofs.Compositional.And]).widen[bifrostModels.Proof].value
        case p: models.ProofCompositionalOr =>
          EitherT(p.toF[F, bifrostModels.Proofs.Compositional.Or]).widen[bifrostModels.Proof].value
        case p: models.ProofCompositionalThreshold =>
          EitherT(p.toF[F, bifrostModels.Proofs.Compositional.Threshold]).widen[bifrostModels.Proof].value
        case p: models.ProofCompositionalNot =>
          EitherT(p.toF[F, bifrostModels.Proofs.Compositional.Not]).widen[bifrostModels.Proof].value
        case p: models.ProofContextualHeightLock =>
          EitherT(p.toF[F, bifrostModels.Proofs.Contextual.HeightLock]).widen[bifrostModels.Proof].value
        case p: models.ProofContextualRequiredTransactionIO =>
          EitherT(p.toF[F, bifrostModels.Proofs.Contextual.RequiredTransactionIO]).widen[bifrostModels.Proof].value
      }
    )
}

trait AddressBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances
    with ProofBifrostMorphismInstances =>

  implicit def spendingAddressIsorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.SpendingAddress, models.SpendingAddress] =
    Isomorphism(
      _.map(p =>
        for {
          typedEvidence <- EitherT(p.typedEvidence.toF[F, models.TypedEvidence])
        } yield models.SpendingAddress(typedEvidence.some)
      ).flatMap(_.value),
      _.map(p =>
        for {
          typedEvidence <- p.typedEvidence
            .toRight("Missing typedEvidence")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.TypedEvidence])
        } yield bifrostModels.SpendingAddress(typedEvidence)
      ).flatMap(_.value)
    )

  implicit def stakingAddressOperatorIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.StakingAddresses.Operator, models.StakingAddressOperator] =
    Isomorphism(
      _.map(p =>
        for {
          vk <- EitherT(p.vk.toF[F, models.VerificationKeyEd25519])
        } yield models.StakingAddressOperator(vk.some)
      ).flatMap(_.value),
      _.map(p =>
        for {
          vk <- p.vk.toRight("Missing vk").toEitherT[F].flatMapF(_.toF[F, bifrostModels.VerificationKeys.Ed25519])
        } yield bifrostModels.StakingAddresses.Operator(vk)
      ).flatMap(_.value)
    )

  implicit def stakingAddressNonStakingIsorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.StakingAddresses.NonStaking.type, models.StakingAddressNonStaking] =
    Isomorphism(
      _.as(models.StakingAddressNonStaking().asRight[String]),
      _.as(bifrostModels.StakingAddresses.NonStaking.asRight[String])
    )

  implicit def stakingAddressIsorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.StakingAddress, models.StakingAddress] =
    Isomorphism(
      _.map {
        case bifrostModels.StakingAddresses.NonStaking =>
          EitherT(bifrostModels.StakingAddresses.NonStaking.toF[F, models.StakingAddressNonStaking])
            .widen[models.StakingAddress]
        case o: bifrostModels.StakingAddresses.Operator =>
          EitherT(o.toF[F, models.StakingAddressOperator]).widen[models.StakingAddress]
      }.flatMap(_.value),
      _.map {
        case o: models.StakingAddressNonStaking =>
          EitherT(o.toF[F, bifrostModels.StakingAddresses.NonStaking.type]).widen[bifrostModels.StakingAddress]
        case o: models.StakingAddressOperator =>
          EitherT(o.toF[F, bifrostModels.StakingAddresses.Operator]).widen[bifrostModels.StakingAddress]
        case models.StakingAddress.Empty =>
          EitherT.leftT[F, bifrostModels.StakingAddress]("Missing stakingAddress")
      }.flatMap(_.value)
    )

  implicit def fullAddressIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.FullAddress, models.FullAddress] =
    Isomorphism(
      _.map(p =>
        for {
          prefix          <- EitherT(p.networkPrefix.toF[F, models.NetworkPrefix])
          spendingAddress <- EitherT(p.spendingAddress.toF[F, models.SpendingAddress])
          stakingAddress  <- EitherT(p.stakingAddress.toF[F, models.StakingAddress])
          commitment      <- EitherT(p.binding.toF[F, models.ProofKnowledgeEd25519])
        } yield models.FullAddress(prefix.some, spendingAddress.some, stakingAddress, commitment.some)
      ).flatMap(_.value),
      _.map(p =>
        for {
          prefix <- p.networkPrefix
            .toRight("Missing networkPrefix")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.NetworkPrefix])
          spendingAddress <- p.spendingAddress
            .toRight("Missing spendingAddress")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.SpendingAddress])
          stakingAddress <- EitherT(p.stakingAddress.toF[F, bifrostModels.StakingAddress])
          commitment <- p.commitment
            .toRight("Missing commitment")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.Ed25519])
        } yield bifrostModels.FullAddress(prefix, spendingAddress, stakingAddress, commitment)
      ).flatMap(_.value)
    )
}

trait CertificateBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances
    with ProofBifrostMorphismInstances =>

  implicit def operationalCertificateIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.OperationalCertificate, models.OperationalCertificate] =
    Isomorphism(
      _.map(a =>
        for {
          parentVK        <- EitherT(a.parentVK.toF[F, models.VerificationKeyKesProduct])
          parentSignature <- EitherT(a.parentSignature.toF[F, models.ProofKnowledgeKesProduct])
          childVK         <- EitherT(a.childVK.toF[F, models.VerificationKeyEd25519])
          childSignature  <- EitherT(a.childSignature.toF[F, models.ProofKnowledgeEd25519])
        } yield models.OperationalCertificate(parentVK.some, parentSignature.some, childVK.some, childSignature.some)
      ).flatMap(_.value),
      _.map(a =>
        for {
          parentVK <- a.parentVK
            .toRight("Missing parentVK")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.VerificationKeys.KesProduct])
          parentSignature <- a.parentSignature
            .toRight("Missing parentSignature")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.KesProduct])
          childVK <- a.childVK
            .toRight("Missing childVK")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.VerificationKeys.Ed25519])
          childSignature <- a.childSignature
            .toRight("Missing childSignature")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.Ed25519])
        } yield bifrostModels.OperationalCertificate(parentVK, parentSignature, childVK, childSignature)
      ).flatMap(_.value)
    )

  implicit def eligibilityCertificateIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.EligibilityCertificate, models.EligibilityCertificate] =
    Isomorphism(
      _.map(a =>
        for {
          vrfSig            <- EitherT(a.vrfSig.toF[F, models.ProofKnowledgeVrfEd25519])
          vkVRF             <- EitherT(a.vkVRF.toF[F, models.VerificationKeyVrfEd25519])
          thresholdEvidence <- EitherT(a.thresholdEvidence.toF[F, ByteString])
          eta               <- EitherT(a.eta.toF[F, ByteString])
        } yield models.EligibilityCertificate(vrfSig.some, vkVRF.some, thresholdEvidence, eta)
      ).flatMap(_.value),
      _.map(a =>
        for {
          vrfSig <- a.vrfSig
            .toRight("Missing vrfSig")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.VrfEd25519])
          vkVRF <- a.vkVRF
            .toRight("Missing vkVRF")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.VerificationKeys.VrfEd25519])
          thresholdEvidence <- EitherT(a.thresholdEvidence.toF[F, bifrostModels.Evidence])
          eta               <- EitherT(a.eta.toF[F, bifrostModels.Eta])
        } yield bifrostModels.EligibilityCertificate(vrfSig, vkVRF, thresholdEvidence, eta)
      ).flatMap(_.value)
    )
}

trait BlockBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with AddressBifrostMorphismInstances
    with CertificateBifrostMorphismInstances =>

  implicit def headerMetadataIsomorphism[F[_]: Monad]
    : Isomorphism[F, Sized.Max[Latin1Data, Lengths.`32`.type], models.BlockHeader.Metadata] =
    Isomorphism(
      fa => EitherT(fa.to[ByteString]).map(models.BlockHeader.Metadata(_)).value,
      _.map(_.value).to[Sized.Max[Latin1Data, Lengths.`32`.type]]
    )

  implicit def headerIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.BlockHeaderV2, models.BlockHeader] =
    Isomorphism(
      _.map(header =>
        for {
          address                <- EitherT(header.address.toF[F, models.StakingAddressOperator])
          parentHeaderId         <- EitherT(header.parentHeaderId.toF[F, models.BlockId])
          eligibilityCertificate <- EitherT(header.eligibilityCertificate.toF[F, models.EligibilityCertificate])
          operationalCertificate <- EitherT(header.operationalCertificate.toF[F, models.OperationalCertificate])
          metadata               <- header.metadata.traverse(v => EitherT(v.toF[F, models.BlockHeader.Metadata]))
        } yield models
          .BlockHeader(
            parentHeaderId.some,
            header.parentSlot,
            header.txRoot.data,
            header.bloomFilter.data,
            header.timestamp,
            header.height,
            header.slot,
            eligibilityCertificate.some,
            operationalCertificate.some,
            metadata,
            address.some
          )
      ).flatMap(_.value),
      _.map(protoHeader =>
        for {
          parentHeaderId <- protoHeader.parentHeaderId
            .toRight("Missing parentHeaderId")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
          txRoot <- EitherT(protoHeader.txRoot.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`32`.type]])
          bloomFilter <- EitherT(
            protoHeader.bloomFilter.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`256`.type]]
          )
          eligibilityCertificate <- protoHeader.eligibilityCertificate
            .toRight("Missing eligibilityCertificate")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.EligibilityCertificate])
          operationalCertificate <- protoHeader.operationalCertificate
            .toRight("Missing operationalCertificate")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.OperationalCertificate])
          metadata <- protoHeader.metadata.traverse(metadata =>
            EitherT(metadata.toF[F, Sized.Max[Latin1Data, Lengths.`32`.type]])
          )
          address <- EitherT
            .fromEither[F](protoHeader.address.toRight("missing address"))
            .flatMapF(_.toF[F, bifrostModels.StakingAddresses.Operator])
        } yield bifrostModels.BlockHeaderV2(
          parentHeaderId,
          protoHeader.parentSlot,
          txRoot,
          bloomFilter,
          protoHeader.timestamp,
          protoHeader.height,
          protoHeader.slot,
          eligibilityCertificate,
          operationalCertificate,
          metadata,
          address
        )
      )
        .flatMap(_.value)
    )

  implicit def bodyIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.BlockBodyV2, models.BlockBody] =
    Isomorphism(
      _.map(body =>
        for {
          transactionIds <- EitherT(body.toList.traverse(_.toF[F, models.TransactionId]).map(_.sequence))
        } yield models.BlockBody(transactionIds)
      ).flatMap(_.value),
      _.map(protoBody =>
        for {
          transactionIds <- EitherT(
            protoBody.transactionIds.toList.traverse(_.toF[F, bifrostModels.TypedIdentifier]).map(_.sequence)
          )
        } yield ListSet.empty[bifrostModels.TypedIdentifier] ++ transactionIds
      ).flatMap(_.value)
    )

}
