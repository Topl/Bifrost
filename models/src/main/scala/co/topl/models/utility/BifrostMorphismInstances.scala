package co.topl.models.utility

import cats.{Functor, Monad}
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.implicits._
import co.topl.models.utility
import co.topl.models.utility.HasLength.instances.{bigIntLength, _}
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{HasLength, Isomorphism, Lengths, Morphism, Sized}
import co.topl.{models => bifrostModels}
import com.google.protobuf.ByteString
import co.topl.proto.models

import scala.collection.immutable.ListSet

trait BifrostMorphismInstances
    extends PrimitiveBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with PropositionBifrostMorphismInstances
    with ProofBifrostMorphismInstances
    with BoxBifrostMorphismInstances
    with AddressBifrostMorphismInstances
    with TransactionBifrostMorphismInstances
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

  implicit def bigIntIsomorphism[F[_]: Monad]: Isomorphism[F, BigInt, ByteString] =
    Isomorphism(
      _.map(v => ByteString.copyFrom(v.toByteArray).asRight[String]),
      _.map(v => BigInt(v.toByteArray).asRight[String])
    )

  implicit def int128Isomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Int128, models.Int128] =
    Isomorphism(
      fa => EitherT(fa.to[ByteString]).map(models.Int128(_)).value,
      fa => EitherT(fa.map(_.value).to[bifrostModels.Int128]).value
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

  implicit def verificationKeysCurve25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.Curve25519, models.VerificationKeyCurve25519] =
    utility.Isomorphism(
      _.map(p =>
        for {
          value <- EitherT(p.bytes.toF[F, ByteString])
        } yield models.VerificationKeyCurve25519(value)
      ).flatMap(_.value),
      _.map(p =>
        for {
          value <- EitherT(
            p.value.toF[F, Sized.Strict[bifrostModels.Bytes, bifrostModels.VerificationKeys.Ed25519.Length]]
          )
        } yield bifrostModels.VerificationKeys.Curve25519(value)
      ).flatMap(_.value)
    )

  implicit def verificationKeysEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.Ed25519, models.VerificationKeyEd25519] =
    utility.Isomorphism(
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

  implicit def verificationKeysExtendedEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.ExtendedEd25519, models.VerificationKeyExtendedEd25519] =
    utility.Isomorphism(
      _.map(p =>
        for {
          vk        <- EitherT(p.vk.toF[F, models.VerificationKeyEd25519])
          chainCode <- EitherT(p.chainCode.toF[F, ByteString])
        } yield models.VerificationKeyExtendedEd25519(vk.some, chainCode)
      ).flatMap(_.value),
      _.map(p =>
        for {
          vk <- p.vk.toRight("Missing VK").toEitherT[F].flatMapF(_.toF[F, bifrostModels.VerificationKeys.Ed25519])
          chainCode <- EitherT(
            p.chainCode
              .toF[F, Sized.Strict[bifrostModels.Bytes, bifrostModels.VerificationKeys.ExtendedEd25519.ChainCodeLength]]
          )
        } yield bifrostModels.VerificationKeys.ExtendedEd25519(vk, chainCode)
      ).flatMap(_.value)
    )

  implicit def verificationKeysVrfEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.VerificationKeys.VrfEd25519, models.VerificationKeyVrfEd25519] =
    utility.Isomorphism(
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
    utility.Isomorphism(
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
          prefix   <- EitherT.cond[F](p.typePrefix <= Byte.MaxValue, p.typePrefix.toByte, "Invalid typePrefix")
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
      _.map(v => bifrostModels.TypedBytes(bifrostModels.IdentifierTypes.Block.HeaderV2, v.value).asRight[String])
    )

  implicit def blockIdHeaderIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.TypedIdentifier, co.topl.consensus.models.BlockId] =
    Isomorphism(
      _.map(typedIdentifier =>
        co.topl.consensus.models.BlockId(ByteString.copyFrom(typedIdentifier.dataBytes.toArray)).asRight[String]
      ),
      _.map(blockId =>
        bifrostModels
          .TypedBytes(bifrostModels.IdentifierTypes.Block.HeaderV2, scodec.bits.ByteVector(blockId.value.toArray))
          .asRight[String]
      )
    )

  implicit def ioTransaction32Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.TypedIdentifier, co.topl.brambl.models.Identifier.IoTransaction32] =
    Isomorphism(
      _.map(v =>
        co.topl.brambl.models.Identifier.IoTransaction32
          .of(
            co.topl.brambl.models.Evidence.Sized32(
              quivr.models.Digest.Digest32(value = ByteString.copyFrom(v.dataBytes.toArray))
            )
          )
          .asRight[String]
      ),
      _.map(v =>
        bifrostModels
          .TypedBytes(
            bifrostModels.IdentifierTypes.Transaction,
            scodec.bits.ByteVector(v.evidence.digest.value.toByteArray)
          )
          .asRight[String]
      )
    )

}

trait PropositionBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances
    with BoxBifrostMorphismInstances =>

  implicit def permanentlyLockedPropositionIsomorphism[F[_]: Functor]
    : Isomorphism[F, bifrostModels.Propositions.PermanentlyLocked.type, models.PropositionPermanentlyLocked] =
    Isomorphism.constant(bifrostModels.Propositions.PermanentlyLocked, models.PropositionPermanentlyLocked())

  implicit def curve25519PropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Knowledge.Curve25519, models.PropositionKnowledgeCurve25519] =
    Isomorphism(
      fa =>
        EitherT(fa.map(_.key).to[models.VerificationKeyCurve25519])
          .map(_.some)
          .map(models.PropositionKnowledgeCurve25519(_))
          .value,
      _.map(
        _.key
          .toRight("Missing key")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.VerificationKeys.Curve25519])
          .map(bifrostModels.Propositions.Knowledge.Curve25519(_))
      )
        .flatMap(_.value)
    )

  implicit def ed25519PropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Knowledge.Ed25519, models.PropositionKnowledgeEd25519] =
    Isomorphism(
      fa =>
        EitherT(fa.map(_.key).to[models.VerificationKeyEd25519])
          .map(_.some)
          .map(models.PropositionKnowledgeEd25519(_))
          .value,
      _.map(
        _.key
          .toRight("Missing key")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.VerificationKeys.Ed25519])
          .map(bifrostModels.Propositions.Knowledge.Ed25519(_))
      )
        .flatMap(_.value)
    )

  implicit def extendedEd25519PropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Knowledge.ExtendedEd25519, models.PropositionKnowledgeExtendedEd25519] =
    Isomorphism(
      fa =>
        EitherT(fa.map(_.key).to[models.VerificationKeyExtendedEd25519])
          .map(_.some)
          .map(models.PropositionKnowledgeExtendedEd25519(_))
          .value,
      _.map(
        _.key
          .toRight("Missing key")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.VerificationKeys.ExtendedEd25519])
          .map(bifrostModels.Propositions.Knowledge.ExtendedEd25519(_))
      )
        .flatMap(_.value)
    )

  implicit def hashLockPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Knowledge.HashLock, models.PropositionKnowledgeHashLock] =
    Isomorphism(
      fa =>
        EitherT(fa.map(_.valueDigest).to[ByteString])
          .map(models.PropositionKnowledgeHashLock(_))
          .value,
      fa =>
        EitherT(fa.map(_.valueDigest).to[bifrostModels.Digest32])
          .map(bifrostModels.Propositions.Knowledge.HashLock(_))
          .value
    )

  implicit def andPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Compositional.And, models.PropositionCompositionalAnd] =
    utility.Isomorphism(
      _.map(v =>
        for {
          a <- EitherT(v.a.toF[F, models.Proposition])
          b <- EitherT(v.b.toF[F, models.Proposition])
        } yield models.PropositionCompositionalAnd(a, b)
      ).flatMap(_.value),
      _.map(v =>
        for {
          a <- EitherT(v.a.toF[F, bifrostModels.Proposition])
          b <- EitherT(v.b.toF[F, bifrostModels.Proposition])
        } yield bifrostModels.Propositions.Compositional.And(a, b)
      ).flatMap(_.value)
    )

  implicit def orPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Compositional.Or, models.PropositionCompositionalOr] =
    utility.Isomorphism(
      _.map(v =>
        for {
          a <- EitherT(v.a.toF[F, models.Proposition])
          b <- EitherT(v.b.toF[F, models.Proposition])
        } yield models.PropositionCompositionalOr(a, b)
      ).flatMap(_.value),
      _.map(v =>
        for {
          a <- EitherT(v.a.toF[F, bifrostModels.Proposition])
          b <- EitherT(v.b.toF[F, bifrostModels.Proposition])
        } yield bifrostModels.Propositions.Compositional.Or(a, b)
      ).flatMap(_.value)
    )

  implicit def thresholdPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Compositional.Threshold, models.PropositionCompositionalThreshold] =
    Isomorphism(
      _.map(v =>
        for {
          threshold    <- v.threshold.asRight[String].toEitherT[F]
          propositions <- EitherT(v.propositions.toList.traverse(_.toF[F, models.Proposition]).map(_.sequence))
        } yield models.PropositionCompositionalThreshold(threshold, propositions)
      ).flatMap(_.value),
      _.map(v =>
        for {
          threshold    <- v.threshold.asRight[String].toEitherT[F]
          propositions <- EitherT(v.propositions.toList.traverse(_.toF[F, bifrostModels.Proposition]).map(_.sequence))
        } yield bifrostModels.Propositions.Compositional.Threshold(threshold, ListSet.empty ++ propositions)
      ).flatMap(_.value)
    )

  implicit def notPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Compositional.Not, models.PropositionCompositionalNot] =
    utility.Isomorphism(
      _.map(v =>
        for {
          a <- EitherT(v.a.toF[F, models.Proposition])
        } yield models.PropositionCompositionalNot(a)
      ).flatMap(_.value),
      _.map(v =>
        for {
          a <- EitherT(v.a.toF[F, bifrostModels.Proposition])
        } yield bifrostModels.Propositions.Compositional.Not(a)
      ).flatMap(_.value)
    )

  implicit def heightLockPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Propositions.Contextual.HeightLock, models.PropositionContextualHeightLock] =
    Isomorphism(
      _.map(v => models.PropositionContextualHeightLock(v.height).asRight[String]),
      _.map(v => bifrostModels.Propositions.Contextual.HeightLock(v.height).asRight[String])
    )

  implicit def boxLocationPropositionIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.BoxLocation, models.BoxLocation] =
    Isomorphism(
      _.map {
        case bifrostModels.BoxLocations.Input(index) =>
          models.BoxLocation(index.toInt, models.BoxLocation.IO.INPUT)
        case bifrostModels.BoxLocations.Output(index) =>
          models.BoxLocation(index.toInt, models.BoxLocation.IO.OUTPUT)
      }.map(_.asRight[String]),
      _.map(location =>
        Either
          .cond(location.index <= Short.MaxValue, location.index.toShort, "Index out of bounds")
          .flatMap(index =>
            location.location match {
              case models.BoxLocation.IO.INPUT  => bifrostModels.BoxLocations.Input(index).asRight[String]
              case models.BoxLocation.IO.OUTPUT => bifrostModels.BoxLocations.Output(index).asRight[String]
              case _                            => "Invalid location".asLeft[bifrostModels.BoxLocation]
            }
          )
      )
    )

  implicit def requiredTransactionIORequirementPropositionIsomorphism[F[_]: Monad]: Isomorphism[
    F,
    bifrostModels.Propositions.Contextual.RequiredTransactionIO.Requirement,
    models.PropositionContextualRequiredTransactionIO.Requirement
  ] =
    utility.Isomorphism(
      _.map(v =>
        for {
          box      <- EitherT(v.box.toF[F, models.Box])
          location <- EitherT(v.location.toF[F, models.BoxLocation])
        } yield models.PropositionContextualRequiredTransactionIO.Requirement(box.some, location.some)
      ).flatMap(_.value),
      _.map(v =>
        for {
          box      <- v.box.toRight("Missing box").toEitherT[F].flatMapF(_.toF[F, bifrostModels.Box])
          location <- v.location.toRight("Missing location").toEitherT[F].flatMapF(_.toF[F, bifrostModels.BoxLocation])
        } yield bifrostModels.Propositions.Contextual.RequiredTransactionIO.Requirement(box, location)
      ).flatMap(_.value)
    )

  implicit def requiredTransactionIOPropositionIsomorphism[F[_]: Monad]: Isomorphism[
    F,
    bifrostModels.Propositions.Contextual.RequiredTransactionIO,
    models.PropositionContextualRequiredTransactionIO
  ] =
    utility.Isomorphism(
      _.map(v =>
        EitherT(
          v.requirements.toList
            .traverse(_.toF[F, models.PropositionContextualRequiredTransactionIO.Requirement])
            .map(_.sequence)
        ).map(models.PropositionContextualRequiredTransactionIO(_))
      ).flatMap(_.value),
      _.map(v =>
        NonEmptyChain
          .fromSeq(v.requirements)
          .toRight("Empty requirements")
          .toEitherT[F]
          .flatMap(v =>
            EitherT(
              v
                .traverse(_.toF[F, bifrostModels.Propositions.Contextual.RequiredTransactionIO.Requirement])
                .map(_.sequence)
            )
          )
          .map(bifrostModels.Propositions.Contextual.RequiredTransactionIO(_))
      ).flatMap(_.value)
    )

  implicit def propositionIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Proposition, models.Proposition] =
    utility.Isomorphism(
      _.map {
        case bifrostModels.Propositions.PermanentlyLocked =>
          EitherT(bifrostModels.Propositions.PermanentlyLocked.toF[F, models.PropositionPermanentlyLocked])
            .widen[models.Proposition]
        case p: bifrostModels.Propositions.Knowledge.Curve25519 =>
          EitherT(p.toF[F, models.PropositionKnowledgeCurve25519]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Knowledge.Ed25519 =>
          EitherT(p.toF[F, models.PropositionKnowledgeEd25519]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Knowledge.ExtendedEd25519 =>
          EitherT(p.toF[F, models.PropositionKnowledgeExtendedEd25519]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Knowledge.HashLock =>
          EitherT(p.toF[F, models.PropositionKnowledgeHashLock]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Compositional.And =>
          EitherT(p.toF[F, models.PropositionCompositionalAnd]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Compositional.Or =>
          EitherT(p.toF[F, models.PropositionCompositionalOr]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Compositional.Threshold =>
          EitherT(p.toF[F, models.PropositionCompositionalThreshold]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Compositional.Not =>
          EitherT(p.toF[F, models.PropositionCompositionalNot]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Contextual.HeightLock =>
          EitherT(p.toF[F, models.PropositionContextualHeightLock]).widen[models.Proposition]
        case p: bifrostModels.Propositions.Contextual.RequiredTransactionIO =>
          EitherT(p.toF[F, models.PropositionContextualRequiredTransactionIO]).widen[models.Proposition]
      }.flatMap(_.value),
      _.map {
        case models.Proposition.Empty =>
          "Empty proposition".asLeft[bifrostModels.Proposition].toEitherT[F]
        case p: models.PropositionPermanentlyLocked =>
          EitherT(p.toF[F, bifrostModels.Propositions.PermanentlyLocked.type]).widen[bifrostModels.Proposition]
        case p: models.PropositionKnowledgeCurve25519 =>
          EitherT(p.toF[F, bifrostModels.Propositions.Knowledge.Curve25519]).widen[bifrostModels.Proposition]
        case p: models.PropositionKnowledgeEd25519 =>
          EitherT(p.toF[F, bifrostModels.Propositions.Knowledge.Ed25519]).widen[bifrostModels.Proposition]
        case p: models.PropositionKnowledgeExtendedEd25519 =>
          EitherT(p.toF[F, bifrostModels.Propositions.Knowledge.ExtendedEd25519]).widen[bifrostModels.Proposition]
        case p: models.PropositionKnowledgeHashLock =>
          EitherT(p.toF[F, bifrostModels.Propositions.Knowledge.HashLock]).widen[bifrostModels.Proposition]
        case p: models.PropositionCompositionalAnd =>
          EitherT(p.toF[F, bifrostModels.Propositions.Compositional.And]).widen[bifrostModels.Proposition]
        case p: models.PropositionCompositionalOr =>
          EitherT(p.toF[F, bifrostModels.Propositions.Compositional.Or]).widen[bifrostModels.Proposition]
        case p: models.PropositionCompositionalThreshold =>
          EitherT(p.toF[F, bifrostModels.Propositions.Compositional.Threshold]).widen[bifrostModels.Proposition]
        case p: models.PropositionCompositionalNot =>
          EitherT(p.toF[F, bifrostModels.Propositions.Compositional.Not]).widen[bifrostModels.Proposition]
        case p: models.PropositionContextualHeightLock =>
          EitherT(p.toF[F, bifrostModels.Propositions.Contextual.HeightLock]).widen[bifrostModels.Proposition]
        case p: models.PropositionContextualRequiredTransactionIO =>
          EitherT(p.toF[F, bifrostModels.Propositions.Contextual.RequiredTransactionIO])
            .widen[bifrostModels.Proposition]
      }.flatMap(_.value)
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

  implicit def proofsKnowledgeCurve25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.Curve25519, models.ProofKnowledgeCurve25519] =
    utility.Isomorphism(
      _.map(p => models.ProofKnowledgeCurve25519(p.bytes.data).asRight[String]),
      _.flatMap(p =>
        EitherT(p.value.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`64`.type]])
          .map(bifrostModels.Proofs.Knowledge.Curve25519(_))
          .value
      )
    )

  implicit def proofsKnowledgeEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.Ed25519, models.ProofKnowledgeEd25519] =
    utility.Isomorphism(
      _.map(p => models.ProofKnowledgeEd25519(p.bytes.data).asRight[String]),
      _.flatMap(p =>
        EitherT(p.value.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`64`.type]])
          .map(bifrostModels.Proofs.Knowledge.Ed25519(_))
          .value
      )
    )

  implicit def proofsKnowledgeVrfEd25519Isomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.VrfEd25519, models.ProofKnowledgeVrfEd25519] =
    utility.Isomorphism(
      _.map(p => models.ProofKnowledgeVrfEd25519(p.bytes.data).asRight[String]),
      _.flatMap(p =>
        EitherT(p.value.toF[F, Sized.Strict[bifrostModels.Bytes, Lengths.`80`.type]])
          .map(bifrostModels.Proofs.Knowledge.VrfEd25519(_))
          .value
      )
    )

  implicit def proofsKnowledgeKesSumIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Proofs.Knowledge.KesSum, models.ProofKnowledgeKesSum] =
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
      _.flatMap {
        case bifrostModels.Proofs.Undefined =>
          EitherT(bifrostModels.Proofs.Undefined.toF[F, models.ProofUndefined]).widen[models.Proof].value
        case p: bifrostModels.Proofs.Knowledge.Curve25519 =>
          EitherT(p.toF[F, models.ProofKnowledgeCurve25519]).widen[models.Proof].value
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
        case p: models.ProofKnowledgeCurve25519 =>
          EitherT(p.toF[F, bifrostModels.Proofs.Knowledge.Curve25519]).widen[bifrostModels.Proof].value
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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
    utility.Isomorphism(
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

trait BoxBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances with CommonBifrostMorphismInstances with AddressBifrostMorphismInstances =>

  implicit def boxIdIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Box.Id, models.Box.Id] =
    utility.Isomorphism(
      _.map(boxId =>
        for {
          transactionId <- EitherT(boxId.transactionId.toF[F, models.TransactionId])
          transactionOutputIndex = boxId.transactionOutputIndex.toInt
        } yield models.Box.Id(transactionId.some, transactionOutputIndex)
      ).flatMap(_.value),
      _.map(protoBoxId =>
        for {
          transactionId <- protoBoxId.transactionId
            .toRight("Missing transactionId")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.TypedIdentifier])
          transactionOutputIndex <- EitherT.cond[F](
            protoBoxId.transactionOutputIndex <= Short.MaxValue,
            protoBoxId.transactionOutputIndex.toShort,
            "transactionOutputIndex out of bounds"
          )
        } yield bifrostModels.Box.Id(transactionId, transactionOutputIndex)
      )
        .flatMap(_.value)
    )

  implicit def boxValueEmptyIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Box.Values.Empty.type, models.EmptyBoxValue] =
    Isomorphism.constant(bifrostModels.Box.Values.Empty, models.EmptyBoxValue())

  implicit def boxValuePolyIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Box.Values.Poly, models.PolyBoxValue] =
    Isomorphism(
      fa => EitherT(fa.map(_.quantity).to[models.Int128]).map(_.some).map(models.PolyBoxValue(_)).value,
      _.map(
        _.quantity
          .toRight("Missing quantity")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.Int128])
          .map(bifrostModels.Box.Values.Poly(_))
      ).flatMap(_.value)
    )

  implicit def boxValueArbitIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Box.Values.Arbit, models.ArbitBoxValue] =
    Isomorphism(
      fa => EitherT(fa.map(_.quantity).to[models.Int128]).map(_.some).map(models.ArbitBoxValue(_)).value,
      _.map(
        _.quantity
          .toRight("Missing quantity")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.Int128])
          .map(bifrostModels.Box.Values.Arbit(_))
      ).flatMap(_.value)
    )

  implicit def boxValueAssetCodeIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Box.Values.AssetV1.Code, models.AssetV1BoxValue.Code] =
    utility.Isomorphism(
      _.map(v =>
        for {
          issuerAddress <- EitherT(v.issuer.toF[F, models.SpendingAddress])
          shortName     <- EitherT(v.shortName.toF[F, ByteString])
        } yield models.AssetV1BoxValue.Code(issuerAddress.some, shortName)
      ).flatMap(_.value),
      _.map(v =>
        for {
          issuerAddress <- v.issuerAddress
            .toRight("Missing issuerAddress")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.SpendingAddress])
          shortName <- EitherT(v.shortName.toF[F, bifrostModels.Box.Values.AssetV1.Code.ShortName])
        } yield bifrostModels.Box.Values.AssetV1.Code(issuerAddress, shortName)
      ).flatMap(_.value)
    )

  implicit def boxValueAssetIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Box.Values.AssetV1, models.AssetV1BoxValue] =
    utility.Isomorphism(
      _.map(v =>
        for {
          quantity     <- EitherT(v.quantity.toF[F, models.Int128])
          assetCode    <- EitherT(v.assetCode.toF[F, models.AssetV1BoxValue.Code])
          securityRoot <- EitherT(v.securityRoot.toF[F, ByteString])
          metadata     <- v.metadata.fold(EitherT.pure[F, String](ByteString.EMPTY))(v => EitherT(v.toF[F, ByteString]))
        } yield models.AssetV1BoxValue(quantity.some, assetCode.some, securityRoot, metadata)
      ).flatMap(_.value),
      _.map(v =>
        for {
          quantity <- v.quantity.toRight("Missing quantity").toEitherT[F].flatMapF(_.toF[F, bifrostModels.Int128])
          assetCode <- v.assetCode
            .toRight("Missing assetCode")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.Box.Values.AssetV1.Code])
          securityRoot <- EitherT(v.securityRoot.toF[F, bifrostModels.Box.Values.AssetV1.SecurityRoot])
          metadata <-
            if (v.metadata.isEmpty) EitherT.pure[F, String](none[bifrostModels.Box.Values.AssetV1.Metadata])
            else EitherT(v.metadata.toF[F, bifrostModels.Box.Values.AssetV1.Metadata]).map(_.some)
        } yield bifrostModels.Box.Values.AssetV1(quantity, assetCode, securityRoot, metadata)
      ).flatMap(_.value)
    )

  implicit def boxValueOperatorRegistrationIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Box.Values.Registrations.Operator, models.OperatorRegistrationBoxValue] =
    Isomorphism(
      fa =>
        EitherT(fa.map(_.vrfCommitment).to[models.ProofKnowledgeKesProduct])
          .map(_.some)
          .map(models.OperatorRegistrationBoxValue(_))
          .value,
      _.map(
        _.vrfCommitment
          .toRight("Missing vrfCommitment")
          .toEitherT[F]
          .flatMapF(_.toF[F, bifrostModels.Proofs.Knowledge.KesProduct])
          .map(bifrostModels.Box.Values.Registrations.Operator(_))
      ).flatMap(_.value)
    )

  implicit def boxValueIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Box.Value, models.BoxValue] =
    utility.Isomorphism(
      _.map {
        case bifrostModels.Box.Values.Empty =>
          EitherT(bifrostModels.Box.Values.Empty.toF[F, models.EmptyBoxValue]).widen[models.BoxValue]
        case p: bifrostModels.Box.Values.Poly =>
          EitherT(p.toF[F, models.PolyBoxValue]).widen[models.BoxValue]
        case p: bifrostModels.Box.Values.Arbit =>
          EitherT(p.toF[F, models.ArbitBoxValue]).widen[models.BoxValue]
        case p: bifrostModels.Box.Values.AssetV1 =>
          EitherT(p.toF[F, models.AssetV1BoxValue]).widen[models.BoxValue]
        case p: bifrostModels.Box.Values.Registrations.Operator =>
          EitherT(p.toF[F, models.OperatorRegistrationBoxValue]).widen[models.BoxValue]
      }.flatMap(_.value),
      _.map {
        case models.BoxValue.Empty =>
          EitherT.leftT[F, bifrostModels.Box.Value]("Missing boxValue")
        case p: models.EmptyBoxValue =>
          EitherT(p.toF[F, bifrostModels.Box.Values.Empty.type]).widen[bifrostModels.Box.Value]
        case p: models.PolyBoxValue =>
          EitherT(p.toF[F, bifrostModels.Box.Values.Poly]).widen[bifrostModels.Box.Value]
        case p: models.ArbitBoxValue =>
          EitherT(p.toF[F, bifrostModels.Box.Values.Arbit]).widen[bifrostModels.Box.Value]
        case p: models.AssetV1BoxValue =>
          EitherT(p.toF[F, bifrostModels.Box.Values.AssetV1]).widen[bifrostModels.Box.Value]
        case p: models.OperatorRegistrationBoxValue =>
          EitherT(p.toF[F, bifrostModels.Box.Values.Registrations.Operator]).widen[bifrostModels.Box.Value]
      }.flatMap(_.value)
    )

  implicit def boxIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Box, models.Box] =
    utility.Isomorphism(
      _.map(p =>
        for {
          evidence <- EitherT(p.evidence.toF[F, models.TypedEvidence])
          value    <- EitherT(p.value.toF[F, models.BoxValue])
        } yield models.Box(evidence.some, value)
      ).flatMap(_.value),
      _.map(p =>
        for {
          evidence <- p.evidence
            .toRight("Missing evidence")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.TypedEvidence])
          value <- EitherT(p.value.toF[F, bifrostModels.Box.Value])
        } yield bifrostModels.Box(evidence, value)
      ).flatMap(_.value)
    )
}

trait TransactionBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with PropositionBifrostMorphismInstances
    with ProofBifrostMorphismInstances
    with BoxBifrostMorphismInstances =>

  implicit def transactionInputIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Transaction.Input, models.Transaction.Input] =
    Isomorphism(
      _.map(input =>
        for {
          boxId       <- input.boxId.asRight[String].toEitherT[F].flatMapF(_.toF[F, models.Box.Id])
          proposition <- input.proposition.asRight[String].toEitherT[F].flatMapF(_.toF[F, models.Proposition])
          proof       <- input.proof.asRight[String].toEitherT[F].flatMapF(_.toF[F, models.Proof])
          value       <- input.value.asRight[String].toEitherT[F].flatMapF(_.toF[F, models.BoxValue])
        } yield models.Transaction.Input(boxId.some, proposition, proof, value)
      )
        .flatMap(_.value),
      _.map(input =>
        for {
          boxId       <- input.boxId.toRight("Missing boxId").toEitherT[F].flatMapF(_.toF[F, bifrostModels.Box.Id])
          proposition <- input.proposition.asRight[String].toEitherT[F].flatMapF(_.toF[F, bifrostModels.Proposition])
          proof       <- input.proof.asRight[String].toEitherT[F].flatMapF(_.toF[F, bifrostModels.Proof])
          value       <- input.value.asRight[String].toEitherT[F].flatMapF(_.toF[F, bifrostModels.Box.Value])
        } yield bifrostModels.Transaction.Input(boxId, proposition, proof, value)
      )
        .flatMap(_.value)
    )

  implicit def transactionOutputIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Transaction.Output, models.Transaction.UnspentOutput] =
    utility.Isomorphism(
      _.map(output =>
        for {
          address <- EitherT(output.address.toF[F, models.FullAddress])
          value   <- EitherT(output.value.toF[F, models.BoxValue])
          minting = output.minting
        } yield models.Transaction.UnspentOutput(address.some, value, minting)
      )
        .flatMap(_.value),
      _.map(output =>
        for {
          address <- output.address
            .toRight("Missing address")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.FullAddress])
          value <- EitherT(output.value.toF[F, bifrostModels.Box.Value])
          minting = output.minting
        } yield bifrostModels.Transaction.Output(address, value, minting)
      ).flatMap(_.value)
    )

  implicit def transactionScheduleIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.Transaction.Schedule, models.Transaction.Schedule] =
    Isomorphism(
      _.map(input => models.Transaction.Schedule(input.creation, input.minimumSlot, input.maximumSlot).asRight[String]),
      _.map(protoInput =>
        bifrostModels.Transaction
          .Schedule(protoInput.creation, protoInput.minimumSlot, protoInput.maximumSlot)
          .asRight[String]
      )
    )

  implicit def transactionIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.Transaction, models.Transaction] =
    utility.Isomorphism(
      _.map(transaction =>
        for {
          inputs <- EitherT(
            transaction.inputs.toList
              .traverse(_.toF[F, models.Transaction.Input])
              .map(_.sequence)
          )
          outputs <- EitherT(
            transaction.outputs.toList
              .traverse(_.toF[F, models.Transaction.UnspentOutput])
              .map(_.sequence)
          )
          schedule <- EitherT(
            transaction.schedule.toF[F, models.Transaction.Schedule]
          )
          data <- transaction.data.fold(EitherT.pure[F, String](ByteString.EMPTY))(v => EitherT(v.toF[F, ByteString]))
        } yield models.Transaction(inputs, outputs, schedule.some, data)
      )
        .flatMap(_.value),
      _.map(protoTransaction =>
        for {
          inputs <- EitherT(
            Chain
              .fromSeq(protoTransaction.inputs)
              .traverse(_.toF[F, bifrostModels.Transaction.Input])
              .map(_.sequence)
          )
          outputs <- EitherT(
            Chain
              .fromSeq(protoTransaction.outputs)
              .traverse(_.toF[F, bifrostModels.Transaction.Output])
              .map(_.sequence)
          )
          schedule <- EitherT
            .fromOption[F](protoTransaction.schedule, "Missing schedule")
            .flatMapF(_.toF[F, bifrostModels.Transaction.Schedule])
          data <-
            if (protoTransaction.data.isEmpty) EitherT.pure[F, String](none[bifrostModels.Transaction.DataTetra])
            else EitherT(protoTransaction.data.toF[F, bifrostModels.Transaction.DataTetra]).map(_.some)
        } yield bifrostModels.Transaction(inputs, outputs, schedule, data)
      )
        .flatMap(_.value)
    )
}

trait CertificateBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with VerificationKeyBifrostMorphismInstances
    with ProofBifrostMorphismInstances =>

  implicit def operationalCertificateIsomorphism[F[_]: Monad]
    : Isomorphism[F, bifrostModels.OperationalCertificate, models.OperationalCertificate] =
    utility.Isomorphism(
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
    utility.Isomorphism(
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
          vrfVK <- a.vrfVK // a.vkVRF
            .toRight("Missing vkVRF")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.VerificationKeys.VrfEd25519])
          thresholdEvidence <- EitherT(a.thresholdEvidence.toF[F, bifrostModels.Evidence])
          eta               <- EitherT(a.eta.toF[F, bifrostModels.Eta])
        } yield bifrostModels.EligibilityCertificate(vrfSig, vrfVK, thresholdEvidence, eta)
      ).flatMap(_.value)
    )
}

trait BlockBifrostMorphismInstances {
  self: PrimitiveBifrostMorphismInstances
    with CommonBifrostMorphismInstances
    with AddressBifrostMorphismInstances
    with CertificateBifrostMorphismInstances =>

  implicit def headerIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.BlockHeader, models.BlockHeader] =
    utility.Isomorphism(
      _.map(header =>
        for {
          address                <- EitherT(header.address.toF[F, models.StakingAddressOperator])
          parentHeaderId         <- EitherT(header.parentHeaderId.toF[F, models.BlockId])
          eligibilityCertificate <- EitherT(header.eligibilityCertificate.toF[F, models.EligibilityCertificate])
          operationalCertificate <- EitherT(header.operationalCertificate.toF[F, models.OperationalCertificate])
          metadata <- header.metadata.fold(EitherT.pure[F, String](ByteString.EMPTY))(v =>
            EitherT(v.toF[F, ByteString])
          )
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
          txRoot <- EitherT(protoHeader.txRoot.toF[F, bifrostModels.TxRoot])
          bloomFilter <- EitherT(
            protoHeader.bloomFilter.toF[F, bifrostModels.BloomFilter]
          )
          eligibilityCertificate <- protoHeader.eligibilityCertificate
            .toRight("Missing eligibilityCertificate")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.EligibilityCertificate])
          operationalCertificate <- protoHeader.operationalCertificate
            .toRight("Missing operationalCertificate")
            .toEitherT[F]
            .flatMapF(_.toF[F, bifrostModels.OperationalCertificate])
          metadata <-
            if (protoHeader.metadata.isEmpty) EitherT.pure[F, String](none[bifrostModels.BlockHeader.Metadata])
            else EitherT(protoHeader.metadata.toF[F, bifrostModels.BlockHeader.Metadata]).map(_.some)
          address <- EitherT
            .fromEither[F](protoHeader.address.toRight("missing address"))
            .flatMapF(_.toF[F, bifrostModels.StakingAddresses.Operator])
        } yield bifrostModels.BlockHeader(
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

  implicit def bodyIsomorphism[F[_]: Monad]: Isomorphism[F, bifrostModels.BlockBody, models.BlockBody] =
    utility.Isomorphism(
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
