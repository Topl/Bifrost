package co.topl.credential

import cats.implicits._
import co.topl.crypto.signing.Ed25519
import co.topl.models.Propositions.Knowledge
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import scodec.{Attempt, DecodeResult, Decoder, Encoder, Err}

import scala.collection.immutable.ListSet

/**
 * An addressable entity for proving transactions
 */
trait Credential {
  def prove(currentProof: Proof): Proof
  def proposition: Proposition
}

object Credential {

  case object False extends Credential {
    def prove(currentProof: Proof): Proof = Proofs.False

    def proposition: Proposition = ???
  }

  object Signing {

    case class Curve25519(
      sk:          SecretKeys.Curve25519,
      transaction: Transaction
    ) extends Credential {

      val proposition: Propositions.Knowledge.Curve25519 =
        sk.proposition[Propositions.Knowledge.Curve25519]

      def prove(currentProof: Proof): Proof =
        Prover[(SecretKeys.Curve25519, Transaction), Proofs.Knowledge.Curve25519]
          .proveWith((sk, transaction))
    }

    case class Ed25519(
      sk:               SecretKeys.Ed25519,
      transaction:      Transaction
    )(implicit ed25519: co.topl.crypto.signing.Ed25519)
        extends Credential {

      val proposition: Propositions.Knowledge.Ed25519 =
        sk.proposition[Propositions.Knowledge.Ed25519]

      def prove(currentProof: Proof): Proof =
        Prover[(SecretKeys.Ed25519, Transaction), Proofs.Knowledge.Ed25519]
          .proveWith((sk, transaction))
    }

    case class ExtendedEd25519(
      sk:                       SecretKeys.ExtendedEd25519,
      transaction:              Transaction
    )(implicit extendedEd25519: co.topl.crypto.signing.ExtendedEd25519)
        extends Credential {

      val proposition: Propositions.Knowledge.ExtendedEd25519 =
        sk.proposition[Propositions.Knowledge.ExtendedEd25519]

      def prove(currentProof: Proof): Proof =
        Prover[(SecretKeys.ExtendedEd25519, Transaction), Proofs.Knowledge.Ed25519]
          .proveWith((sk, transaction))
    }
  }

  object Compositional {

    case class Threshold(proposition: Propositions.Compositional.Threshold, credentials: Iterable[Credential])
        extends Credential {

      def prove(currentProof: Proof): Proof =
        currentProof match {
          case t: Proofs.Compositional.Threshold =>
            Proofs.Compositional.Threshold(
              ListSet.from(
                proposition.propositions.toList
                  .zip(t.proofs)
                  .map {
                    case (prop, Proofs.False) =>
                      credentials.find(_.proposition == prop).fold(Proofs.False: Proof)(_.prove(Proofs.False))
                    case (_, proof) =>
                      proof
                  }
              )
            )
          case _ =>
            Proofs.Compositional.Threshold(
              ListSet.from(
                proposition.propositions.toList.map(prop =>
                  credentials.find(_.proposition == prop).fold(Proofs.False: Proof)(_.prove(Proofs.False))
                )
              )
            )
        }
    }

    case class And(proposition: Propositions.Compositional.And, credentials: Iterable[Credential]) extends Credential {

      def prove(currentProof: Proof): Proofs.Compositional.And =
        currentProof match {
          case t: Proofs.Compositional.And =>
            Proofs.Compositional.And(
              t.a match {
                case Proofs.False =>
                  credentials.find(_.proposition == proposition.a).fold(Proofs.False: Proof)(_.prove(Proofs.False))
                case a =>
                  a
              },
              t.b match {
                case Proofs.False =>
                  credentials.find(_.proposition == proposition.b).fold(Proofs.False: Proof)(_.prove(Proofs.False))
                case b =>
                  b
              }
            )
          case _ =>
            Proofs.Compositional.And(
              credentials.find(_.proposition == proposition.a).fold(Proofs.False: Proof)(_.prove(Proofs.False)),
              credentials.find(_.proposition == proposition.b).fold(Proofs.False: Proof)(_.prove(Proofs.False))
            )
        }
    }

    case class Or(proposition: Propositions.Compositional.Or, credentials: Iterable[Credential]) extends Credential {

      def prove(currentProof: Proof): Proof =
        currentProof match {
          case t: Proofs.Compositional.Or =>
            Proofs.Compositional.Or(
              t.a match {
                case Proofs.False =>
                  credentials.find(_.proposition == proposition.a).fold(Proofs.False: Proof)(_.prove(Proofs.False))
                case a =>
                  a
              },
              t.b match {
                case Proofs.False =>
                  credentials.find(_.proposition == proposition.b).fold(Proofs.False: Proof)(_.prove(Proofs.False))
                case b =>
                  b
              }
            )
          case _ =>
            Proofs.Compositional.Or(
              credentials.find(_.proposition == proposition.a).fold(Proofs.False: Proof)(_.prove(Proofs.False)),
              credentials.find(_.proposition == proposition.b).fold(Proofs.False: Proof)(_.prove(Proofs.False))
            )
        }
    }
  }

  object Contextual {

    case class HeightLock(proposition: Propositions.Contextual.HeightLock, blockId: TypedIdentifier)
        extends Credential {
      def prove(currentProof: Proof): Proof = Proofs.Contextual.HeightLock(blockId)
    }
  }

  trait Instances {

    // TODO: Move to byte-codecs

    implicit val curve25519SKEncoder: Encoder[SecretKeys.Curve25519] =
      Encoder(t =>
        Attempt.successful(
          TypedBytes(1: Byte, t.bytes.data).allBytes.toBitVector
        )
      )

    implicit val curve25519SKDecoder: Decoder[SecretKeys.Curve25519] =
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
          TypedBytes(5: Byte, t.leftKey.data ++ t.rightKey.data ++ t.chainCode.data).allBytes.toBitVector
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
