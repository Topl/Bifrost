package co.topl.typeclasses

import cats._
import cats.data.OptionT
import cats.implicits._
import co.topl.codecs.json.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.typeclasses.implicits._
import io.circe.Json
import io.circe.syntax._

import scala.language.implicitConversions

trait ProofVerifier[F[_]] {

  /**
   * Does the given `proof` satisfy the given `proposition` using the given `data`?
   */
  def verifyWith(proposition: Proposition, proof: Proof, context: VerificationContext[F]): F[Boolean]
}

object ProofVerifier {

  trait Implicits {

    implicit class ProofOps(proof: Proof) {

      def satisfies[F[_]](
        proposition:      Proposition
      )(implicit context: VerificationContext[F], ev: ProofVerifier[F]): F[Boolean] =
        ev.verifyWith(proposition, proof, context)
    }

    implicit class PropositionOps(proposition: Proposition) {

      def isSatisfiedBy[F[_]](
        proof:            Proof
      )(implicit context: VerificationContext[F], ev: ProofVerifier[F]): F[Boolean] =
        ev.verifyWith(proposition, proof, context)
    }
  }

  object ops extends Implicits

  trait Instances {
    private val curve25519 = new Curve25519()

    private def publicKeyCurve25519Verifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.Curve25519,
      proof:       Proofs.Knowledge.Curve25519,
      context:     VerificationContext[F]
    ): F[Boolean] =
      curve25519
        .verify(
          proof,
          context.currentTransaction.signableBytes,
          proposition.key
        )
        .pure[F]

    implicit def publicKeyEd25519Verifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.Ed25519,
      proof:       Proofs.Knowledge.Ed25519,
      context:     VerificationContext[F]
    )(implicit
      ed25519: Ed25519
    ): F[Boolean] =
      ed25519
        .verify(
          proof,
          context.currentTransaction.signableBytes,
          proposition.key
        )
        .pure[F]

    private def publicKeyExtendedEd25519Verifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.ExtendedEd25519,
      proof:       Proofs.Knowledge.Ed25519,
      context:     VerificationContext[F]
    )(implicit
      extendedEd25519: ExtendedEd25519
    ): F[Boolean] =
      extendedEd25519
        .verify(
          proof,
          context.currentTransaction.signableBytes,
          proposition.key
        )
        .pure[F]

    private def heightLockVerifier[F[_]: Applicative](
      proposition: Propositions.Contextual.HeightLock,
      context:     VerificationContext[F]
    ): F[Boolean] = (context.currentHeight >= proposition.height).pure[F]

//    private def requiredOutputVerifier[F[_]: Applicative](
//      proposition: Propositions.Contextual.RequiredDionOutput,
//      context:     VerificationContext[F]
//    ): F[Boolean] =
//      (context.currentTransaction.coinOutputs
//        .toList(proposition.index)
//        .dionAddress(NetworkPrefix(0)) == proposition.address).pure[F]

    private def enumeratedOutputVerifier[F[_]: Applicative](
      proposition: Propositions.Example.EnumeratedInput,
      proof:       Proofs.Example.EnumeratedInput
    ): F[Boolean] = proposition.values.contains(proof.value).pure[F]

    private def hashLockVerifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.HashLock,
      proof:       Proofs.Knowledge.HashLock
    ): F[Boolean] =
      (blake2b256.hash(proof.salt.data.toArray :+ proof.value).value sameElements proposition.digest.data.toArray)
        .pure[F]

    private def requiredBoxVerifier[F[_]: Applicative](
      proposition: Propositions.Contextual.RequiredBoxState,
      context:     VerificationContext[F]
    ): F[Boolean] = {
      def compareBoxes(propositionBox: Box[_])(sourceBox: Box[_]): Boolean = propositionBox match {
        case Box(TypedEvidence.empty, 0, Box.Values.Empty, 0) =>
          false
        case Box(TypedEvidence.empty, 0, Box.Values.Empty, data) =>
          data == sourceBox.data
        case Box(TypedEvidence.empty, 0, value, 0) =>
          value == sourceBox.value
        case Box(TypedEvidence.empty, 0, value, data) =>
          value == sourceBox.value && data == sourceBox.data
        case Box(TypedEvidence.empty, nonce, Box.Values.Empty, 0) =>
          nonce == sourceBox.nonce
        case Box(TypedEvidence.empty, nonce, Box.Values.Empty, data) =>
          nonce == sourceBox.nonce && data == sourceBox.data
        case Box(TypedEvidence.empty, nonce, value, 0) =>
          nonce == sourceBox.nonce && value == sourceBox.value
        case Box(TypedEvidence.empty, nonce, value, data) =>
          nonce == sourceBox.nonce && value == sourceBox.value && data == sourceBox.data
        case Box(typedEvidence, 0, Box.Values.Empty, 0) =>
          typedEvidence == sourceBox.evidence
        case Box(typedEvidence, 0, Box.Values.Empty, data) =>
          typedEvidence == sourceBox.evidence && data == sourceBox.data
        case Box(typedEvidence, 0, value, 0) =>
          typedEvidence == sourceBox.evidence && value == sourceBox.value
        case Box(typedEvidence, 0, value, data) =>
          typedEvidence == sourceBox.evidence && value == sourceBox.value && data == sourceBox.data
        case Box(typedEvidence, nonce, Box.Values.Empty, 0) =>
          typedEvidence == sourceBox.evidence && nonce == sourceBox.nonce
        case Box(typedEvidence, nonce, Box.Values.Empty, data) =>
          typedEvidence == sourceBox.evidence && nonce == sourceBox.nonce && data == sourceBox.data
        case Box(typedEvidence, nonce, value, 0) =>
          typedEvidence == sourceBox.evidence && nonce == sourceBox.nonce && value == sourceBox.value
        case Box(typedEvidence, nonce, value, data) =>
          typedEvidence == sourceBox.evidence && nonce == sourceBox.nonce && value == sourceBox.value && data == sourceBox.data
        case _ => false
      }

      proposition.boxes
        .forall { case (index, box) =>
          proposition.location match {
            case BoxLocations.Input  => compareBoxes(box)(context.inputBoxes(index))
            case BoxLocations.Output => compareBoxes(box)(Box(context.currentTransaction.coinOutputs.toList(index)))
          }
        }
        .pure[F]
    }

    private def thresholdVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.Threshold,
      proof:       Proofs.Compositional.Threshold,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      if (proposition.threshold === 0) true.pure[F]
      else if (proposition.threshold >= proposition.propositions.size) false.pure[F]
      else if (proof.proofs.isEmpty) false.pure[F]
      // We assume a one-to-one pairing of sub-proposition to sub-proof with the assumption that some of the proofs
      // may be Proofs.False
      else if (proof.proofs.size =!= proposition.propositions.size) false.pure[F]
      else {
        proposition.propositions.toList
          .zip(proof.proofs)
          .foldLeftM(0L) {
            case (successCount, _) if successCount >= proposition.threshold =>
              successCount.pure[F]
            case (successCount, (prop, proof)) =>
              proofVerifier.verifyWith(prop, proof, context).map {
                case true => successCount + 1
                case _    => successCount
              }
          }
          .map(_ >= proposition.threshold)
      }

    private def andVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.And,
      proof:       Proofs.Compositional.And,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      proofVerifier
        .verifyWith(proposition.a, proof.a, context)
        .flatMap {
          case true => proofVerifier.verifyWith(proposition.b, proof.b, context)
          case _    => false.pure[F]
        }

    private def orVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.Or,
      proof:       Proofs.Compositional.Or,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      proofVerifier
        .verifyWith(proposition.a, proof.a, context)
        .flatMap {
          case false => proofVerifier.verifyWith(proposition.b, proof.b, context)
          case _     => true.pure[F]
        }

    private def notVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.Not,
      proof:       Proofs.Compositional.Not,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      proofVerifier
        .verifyWith(proposition.a, proof.a, context)
        .map(!_)

    private def jsScriptVerifier[F[_]: Monad](
      proposition: Propositions.Script.JS,
      proof:       Proofs.Script.JS,
      context:     VerificationContext[F],
      jsExecutor:  Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]]
    ): F[Boolean] =
      OptionT
        .fromOption[F](io.circe.parser.parse(proof.serializedArgs).toOption)
        .semiflatMap { argsJson =>
          val contextJson =
            Json.obj(
              "currentTransaction" -> context.currentTransaction.asJson,
              "currentHeight"      -> context.currentHeight.asJson,
              "currentSlot"        -> context.currentSlot.asJson
            )
          jsExecutor(proposition.script)
            .flatMap(f => f(contextJson, argsJson))
        }
        .getOrElse(false)

    implicit def proofVerifier[F[_]: Monad](implicit
      ed25519:         Ed25519,
      extendedEd25519: ExtendedEd25519,
      jsExecutor:      Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]]
    ): ProofVerifier[F] =
      (proposition, proof, context) =>
        (proposition, proof) match {
          case (Propositions.PermanentlyLocked, _) =>
            false.pure[F]
          case (prop: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519) =>
            publicKeyCurve25519Verifier[F](prop, proof, context)
          case (prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519) =>
            publicKeyEd25519Verifier[F](prop, proof, context)
          case (prop: Propositions.Knowledge.ExtendedEd25519, proof: Proofs.Knowledge.Ed25519) =>
            publicKeyExtendedEd25519Verifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.Threshold, proof: Proofs.Compositional.Threshold) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            thresholdVerifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.And, proof: Proofs.Compositional.And) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            andVerifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.Or, proof: Proofs.Compositional.Or) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            orVerifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.Not, proof: Proofs.Compositional.Not) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            notVerifier[F](prop, proof, context)
          case (prop: Propositions.Contextual.HeightLock, _: Proofs.Contextual.HeightLock) =>
            heightLockVerifier[F](prop, context)
          case (prop: Propositions.Contextual.RequiredBoxState, proof: Proofs.Contextual.RequiredBoxState) =>
            requiredBoxVerifier[F](prop, context)
          case (prop: Propositions.Knowledge.HashLock, proof: Proofs.Knowledge.HashLock) =>
            hashLockVerifier[F](prop, proof)
          case (prop: Propositions.Example.EnumeratedInput, proof: Proofs.Example.EnumeratedInput) =>
            enumeratedOutputVerifier[F](prop, proof)
          case (prop: Propositions.Script.JS, proof: Proofs.Script.JS) =>
            jsScriptVerifier[F](prop, proof, context, jsExecutor)
          case _ =>
            false.pure[F]
        }
  }

  object Instances extends Instances
}

trait VerificationContext[F[_]] {
  def currentTransaction: Transaction
  def currentHeight: Long
  def inputBoxes: List[Box[Box.Value]]
  def currentSlot: Slot
}
