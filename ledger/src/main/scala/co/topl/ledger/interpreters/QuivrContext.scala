package co.topl.ledger.interpreters

import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.models.Datum
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.common.ParsableDataInterface
import co.topl.consensus.models.BlockHeader
import co.topl.models.utility._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Ed25519
import co.topl.models.Slot
import co.topl.quivr.algebras.DigestVerifier
import co.topl.quivr.algebras.SignatureVerifier
import co.topl.quivr.runtime.DynamicContext
import co.topl.quivr.runtime.QuivrRuntimeError
import co.topl.quivr.runtime.QuivrRuntimeErrors
import quivr.models.DigestVerification
import quivr.models.SignableBytes
import quivr.models.SignatureVerification

object QuivrContext {

  def forConstructedBlock[F[_]: Sync](
    header:      BlockHeader,
    transaction: IoTransaction
  ): DynamicContext[F, String, Datum] =
    new DynamicContext[F, String, Datum] {
      val datums: String => Option[Datum] = _ => None
      val interfaces: Map[String, ParsableDataInterface] = Map.empty

      lazy val signingRoutines: Map[String, SignatureVerifier[F]] =
        Map("ed25519" -> createEd25519SignatureVerifier())

      lazy val hashingRoutines: Map[String, DigestVerifier[F]] =
        Map("blake2b256" -> createBlake2b256DigestVerifier())

      val signableBytes: F[SignableBytes] = Sync[F].delay(transaction.signable)

      val currentTick: F[Long] =
        header.slot.pure[F]

      def heightOf(label: String): F[Option[Long]] =
        Sync[F].delay(
          label match {
            case "header" => header.height.some
            case _        => None
          }
        )
    }

  private def createEd25519SignatureVerifier[F[_]: Sync](): SignatureVerifier[F] =
    new SignatureVerifier[F] {
      val ed25519 = new Ed25519

      def validate(t: SignatureVerification): F[Either[QuivrRuntimeError, SignatureVerification]] =
        Sync[F].delay(
          Either.cond(
            ed25519.verify(
              t.signature.toByteArray,
              t.message.toByteArray,
              Ed25519.PublicKey(t.verificationKey.toByteArray)
            ),
            t,
            QuivrRuntimeErrors.ValidationError.UserProvidedInterfaceFailure // TODO
          )
        )
    }

  private def createBlake2b256DigestVerifier[F[_]: Sync](): DigestVerifier[F] =
    new DigestVerifier[F] {
      val blake2b256 = new Blake2b256

      def validate(t: DigestVerification): F[Either[QuivrRuntimeError, DigestVerification]] =
        Sync[F].delay(
          Either.cond(
            blake2b256.hash(t.preimage.input.concat(t.preimage.salt)) === t.digest.getDigest32.value,
            t,
            QuivrRuntimeErrors.ValidationError.UserProvidedInterfaceFailure // TODO
          )
        )
    }

  def forProposedBlock[F[_]: Sync](
    height:      Long,
    slot:        Slot,
    transaction: IoTransaction
  ): DynamicContext[F, String, Datum] =
    new DynamicContext[F, String, Datum] {
      val datums: String => Option[Datum] = _ => None
      val interfaces: Map[String, ParsableDataInterface] = Map.empty

      lazy val signingRoutines: Map[String, SignatureVerifier[F]] =
        Map("ed25519" -> createEd25519SignatureVerifier())

      lazy val hashingRoutines: Map[String, DigestVerifier[F]] =
        Map("blake2b256" -> createBlake2b256DigestVerifier())

      val signableBytes: F[SignableBytes] = Sync[F].delay(transaction.signable)

      val currentTick: F[Long] = slot.pure[F]

      def heightOf(label: String): F[Option[Long]] =
        Sync[F].delay(
          label match {
            case "header" => height.some
            case _        => None
          }
        )
    }
}
