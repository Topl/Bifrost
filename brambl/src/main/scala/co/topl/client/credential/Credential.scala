package co.topl.client.credential

import cats.implicits._
import co.topl.crypto.signing.Ed25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import scodec.{Attempt, DecodeResult, Decoder, Encoder, Err}

/**
 * An addressable entity for proving transactions
 */
trait Credential {
  def prove(partiallyProven: Transaction): Transaction
  def address: DionAddress
  private[credential] def secretData: TypedBytes
}

object Credential {

  def apply[T, Prf <: Proof, Prop <: Proposition](
    t: T
  )(implicit
    prover:          Prover[T, Prf],
    proposer:        Proposer[T, Prop],
    dionAddressable: DionAddressable[T],
    networkPrefix:   NetworkPrefix,
    encoder:         Encoder[T]
  ): Credential =
    new Credential {
      val address: DionAddress = dionAddressable.dionAddressOf(t)

      def prove(partiallyProven: Transaction): Transaction = {
        val partialAttestation =
          proposer.propositionOf(t) -> prover.proveWith(t, partiallyProven: Transaction)
        partiallyProven.copy(attestation = partiallyProven.attestation + partialAttestation)
      }

      private[credential] def secretData: TypedBytes =
        TypedBytes(encoder.encode(t).fold(e => throw new IllegalArgumentException(e.message), _.toByteVector))
    }

  trait Instances {

    implicit def credentialDecoder[T, Prf <: Proof, Prop <: Proposition](implicit
      prover:          Prover[T, Prf],
      proposer:        Proposer[T, Prop],
      dionAddressable: DionAddressable[T],
      networkPrefix:   NetworkPrefix,
      tDecoder:        Decoder[T]
    ): Decoder[Credential] =
      Decoder(tBytes =>
        tDecoder
          .decode(tBytes)
          .map(
            _.map(t =>
              new Credential {
                val address: DionAddress = dionAddressable.dionAddressOf(t)

                def prove(partiallyProven: Transaction): Transaction = {
                  val partialAttestation =
                    proposer.propositionOf(t) -> prover.proveWith(t, partiallyProven: Transaction)
                  partiallyProven.copy(attestation = partiallyProven.attestation + partialAttestation)
                }

                private[credential] def secretData: TypedBytes =
                  TypedBytes(tBytes.toByteVector)
              }
            )
          )
      )

    implicit val curve25519SKEncoder: Encoder[SecretKeys.Curve25519] =
      Encoder(t =>
        Attempt.successful(
          TypedBytes(1: Byte, t.bytes.data).allBytes.toBitVector
        )
      )

    implicit def curve25519SKDecoder: Decoder[SecretKeys.Curve25519] =
      bits =>
        Attempt.fromEither(
          (for {
            (r0, _) <- bits.toByteVector.consume(1)(t => Either.cond(t.head === (1: Byte), (), "Invalid type"))
            (r1, key) <- r0.consume(32)(
              Sized.strict[Bytes, SecretKeys.Curve25519.Length](_).leftMap(_ => "Invalid length")
            )
          } yield DecodeResult(SecretKeys.Curve25519(key), r1.toBitVector))
            .leftMap(Err(_))
        )

    implicit val ed25519SKEncoder: Encoder[SecretKeys.Ed25519] =
      Encoder(t =>
        Attempt.successful(
          TypedBytes(3: Byte, t.bytes.data).allBytes.toBitVector
        )
      )

    implicit def ed25519SKDecoder: Decoder[SecretKeys.Ed25519] =
      bits =>
        Attempt.fromEither(
          (for {
            (r0, _) <- bits.toByteVector.consume(1)(t => Either.cond(t.head === (3: Byte), (), "Invalid type"))
            (r1, key) <- r0.consume(32)(
              Sized.strict[Bytes, SecretKeys.Ed25519.Length](_).leftMap(_ => "Invalid length")
            )
          } yield DecodeResult(SecretKeys.Ed25519(key), r1.toBitVector))
            .leftMap(Err(_))
        )

    implicit val extendedEd25519SKEncoder: Encoder[SecretKeys.ExtendedEd25519] =
      Encoder(t =>
        Attempt.successful(
          TypedBytes(5: Byte, (t.leftKey.data ++ t.rightKey.data ++ t.chainCode.data)).allBytes.toBitVector
        )
      )

    implicit def extendedEd25519SKDecoder(implicit ed: Ed25519): Decoder[SecretKeys.ExtendedEd25519] =
      bits =>
        Attempt.fromEither(
          (for {
            (r0, _) <- bits.toByteVector.consume(1)(t => Either.cond(t.head === (5: Byte), (), "Invalid type"))
            (r1, leftKey) <- r0.consume(32)(
              Sized.strict[Bytes, SecretKeys.ExtendedEd25519.LeftLength](_).leftMap(_ => "Invalid length")
            )
            (r2, rightKey) <- r1.consume(32)(
              Sized.strict[Bytes, SecretKeys.ExtendedEd25519.RightLength](_).leftMap(_ => "Invalid length")
            )
            (r3, chainCode) <- r2.consume(32)(
              Sized.strict[Bytes, SecretKeys.ExtendedEd25519.ChainCodeLength](_).leftMap(_ => "Invalid length")
            )
          } yield DecodeResult(SecretKeys.ExtendedEd25519(leftKey, rightKey, chainCode), r3.toBitVector))
            .leftMap(Err(_))
        )
  }

  object instances extends Instances
}
