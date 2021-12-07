package co.topl.credential

import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._

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

    def proposition: Proposition =
      // NOTE: The intention of using this made-up Curve proposition is to force it to not match any other propositions
      Propositions.Knowledge.Curve25519(VerificationKeys.Curve25519(Sized.strictUnsafe(Bytes.fill(32)(-1: Byte))))
  }

  object Signing {

    case class Curve25519(
      sk:                  SecretKeys.Curve25519,
      unprovenTransaction: Transaction.Unproven
    ) extends Credential {

      val proposition: Propositions.Knowledge.Curve25519 =
        sk.vk[VerificationKeys.Curve25519].proposition[Propositions.Knowledge.Curve25519]

      def prove(currentProof: Proof): Proof =
        Prover[(SecretKeys.Curve25519, Transaction.Unproven), Proofs.Knowledge.Curve25519]
          .proveWith((sk, unprovenTransaction))
    }

    case class Ed25519(
      sk:                  SecretKeys.Ed25519,
      unprovenTransaction: Transaction.Unproven
    )(implicit ed25519:    co.topl.crypto.signing.Ed25519)
        extends Credential {

      val proposition: Propositions.Knowledge.Ed25519 =
        sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]

      def prove(currentProof: Proof): Proof =
        Prover[(SecretKeys.Ed25519, Transaction.Unproven), Proofs.Knowledge.Ed25519]
          .proveWith((sk, unprovenTransaction))
    }

    case class ExtendedEd25519(
      sk:                       SecretKeys.ExtendedEd25519,
      unprovenTransaction:      Transaction.Unproven
    )(implicit extendedEd25519: co.topl.crypto.signing.ExtendedEd25519)
        extends Credential {

      val proposition: Propositions.Knowledge.ExtendedEd25519 =
        sk.vk[VerificationKeys.ExtendedEd25519].proposition[Propositions.Knowledge.ExtendedEd25519]

      def prove(currentProof: Proof): Proof =
        Prover[(SecretKeys.ExtendedEd25519, Transaction.Unproven), Proofs.Knowledge.Ed25519]
          .proveWith((sk, unprovenTransaction))
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

    case class HeightLock(minimumHeight: Long) extends Credential {
      def prove(currentProof: Proof): Proof = Proofs.Contextual.HeightLock()
      val proposition: Propositions.Contextual.HeightLock = Propositions.Contextual.HeightLock(minimumHeight)
    }
  }
}
