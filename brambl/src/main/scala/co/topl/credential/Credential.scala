package co.topl.credential

import co.topl.crypto.hash.blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import io.circe.Json

/**
 * An entity which represents a prover for some proposition.
 */
trait Credential {

  /**
   * Modify the given currentProof (potentially Proofs.False) to produce a new proof with some additional
   * information added.  In the simple cases, the simple cases, a basic proof can be returned.  In more complex compositional
   * cases, the given proof should be updated with sub-proofs which can be applied by this credential.  It
   * should not attempt to modify already-proven sub-propositions.
   * @param currentProof The current proof (defaulting to Proofs.False) which should be modified or replaced with a new
   *                     proof
   * @return a new proof
   */
  def prove(currentProof: Proof): Proof

  /**
   * Initializes a new Proof
   */
  def proof: Proof = prove(Proofs.False)

  /**
   * The proposition corresponding to this credential
   * @return
   */
  def proposition: Proposition
}

object Credential {

  case object False extends Credential {
    def prove(currentProof: Proof): Proof = Proofs.False

    def proposition: Proposition = Propositions.PermanentlyLocked
  }

  object Knowledge {

    case class Curve25519(
      sk:                  SecretKeys.Curve25519,
      unprovenTransaction: Transaction.Unproven
    ) extends Credential {
      val proposition: Proposition = sk.vk.asProposition
      def prove(currentProof: Proof): Proof = (sk, unprovenTransaction).asProof
    }

    case class Ed25519(
      sk:                  SecretKeys.Ed25519,
      unprovenTransaction: Transaction.Unproven
    )(implicit ed25519:    co.topl.crypto.signing.Ed25519)
        extends Credential {
      val proposition: Proposition = sk.vk.asProposition
      def prove(currentProof: Proof): Proof = (sk, unprovenTransaction).asProof
    }

    case class ExtendedEd25519(
      sk:                       SecretKeys.ExtendedEd25519,
      unprovenTransaction:      Transaction.Unproven
    )(implicit extendedEd25519: co.topl.crypto.signing.ExtendedEd25519)
        extends Credential {
      val proposition: Proposition = sk.vk.asProposition
      def prove(currentProof: Proof): Proof = (sk, unprovenTransaction).asProof
    }

    case class HashLock(salt: Digest32, value: Byte) extends Credential {
      override def prove(currentProof: Proof): Proof = Proofs.Knowledge.HashLock(salt, value)

      override def proposition: Proposition =
        Propositions.Knowledge.HashLock(Sized.strictUnsafe(Bytes(blake2b256.hash(salt.data.toArray :+ value).value)))
    }
  }

  object Compositional {

    case class Threshold(proposition: Propositions.Compositional.Threshold, credentials: Iterable[Credential])
        extends Credential {

      def prove(currentProof: Proof): Proof =
        currentProof match {
          case t: Proofs.Compositional.Threshold =>
            Proofs.Compositional.Threshold(
              proposition.propositions.toList
                .zip(t.proofs)
                .map { case (prop, proof) => compositionalProver(prop, proof, credentials) }
            )
          case _ =>
            Proofs.Compositional.Threshold(
              proposition.propositions.toList.map(prop => compositionalProver(prop, Proofs.False, credentials))
            )
        }
    }

    case class And(proposition: Propositions.Compositional.And, credentials: Iterable[Credential]) extends Credential {

      def prove(currentProof: Proof): Proofs.Compositional.And =
        currentProof match {
          case t: Proofs.Compositional.And =>
            Proofs.Compositional.And(
              compositionalProver(proposition.a, t.a, credentials),
              compositionalProver(proposition.b, t.b, credentials)
            )
          case _ =>
            Proofs.Compositional.And(
              compositionalProver(proposition.a, Proofs.False, credentials),
              compositionalProver(proposition.b, Proofs.False, credentials)
            )
        }
    }

    case class Or(proposition: Propositions.Compositional.Or, credentials: Iterable[Credential]) extends Credential {

      def prove(currentProof: Proof): Proof =
        currentProof match {
          case t: Proofs.Compositional.Or =>
            Proofs.Compositional.Or(
              compositionalProver(proposition.a, t.a, credentials),
              compositionalProver(proposition.b, t.b, credentials)
            )
          case _ =>
            Proofs.Compositional.Or(
              compositionalProver(proposition.a, Proofs.False, credentials),
              compositionalProver(proposition.b, Proofs.False, credentials)
            )
        }
    }

    case class Not(proposition: Propositions.Compositional.Not, credentials: Iterable[Credential]) extends Credential {

      def prove(currentProof: Proof): Proof =
        currentProof match {
          case t: Proofs.Compositional.Not =>
            Proofs.Compositional.Not(
              compositionalProver(proposition.a, t.a, credentials)
            )
          case _ =>
            Proofs.Compositional.Not(
              compositionalProver(proposition.a, Proofs.False, credentials)
            )
        }
    }

    private def compositionalProver(
      proposition:  Proposition,
      currentProof: Proof,
      credentials:  Iterable[Credential]
    ): Proof =
      (proposition, currentProof) match {
        case (Propositions.PermanentlyLocked, _) =>
          Proofs.False
        case (a: Propositions.Compositional.And, proof) =>
          Credential.Compositional.And(a, credentials).prove(proof)
        case (o: Propositions.Compositional.Or, proof) =>
          Credential.Compositional.Or(o, credentials).prove(proof)
        case (o: Propositions.Compositional.Not, proof) =>
          Credential.Compositional.Not(o, credentials).prove(proof)
        case (t: Propositions.Compositional.Threshold, proof) =>
          Credential.Compositional.Threshold(t, credentials).prove(proof)
        case (prop, Proofs.False) =>
          credentials.find(_.proposition == prop).fold(Proofs.False: Proof)(_.proof)
        case (_, proof) =>
          proof
      }
  }

  object Contextual {

    case class HeightLock(minimumHeight: Long) extends Credential {
      def prove(currentProof: Proof): Proof = Proofs.Contextual.HeightLock()
      val proposition: Propositions.Contextual.HeightLock = Propositions.Contextual.HeightLock(minimumHeight)
    }

//    case class RequiredDionOutput(index: Int, address: DionAddress) extends Credential {
//      def prove(currentProof: Proof): Proof = Proofs.Contextual.RequiredOutput()
//
//      val proposition: Propositions.Contextual.RequiredDionOutput =
//        Propositions.Contextual.RequiredDionOutput(index, address)
//    }

    case class RequiredBoxState(location: BoxLocation, boxes: List[(Int, Box[Box.Value])]) extends Credential {
      def prove(currentProof: Proof): Proof = Proofs.Contextual.RequiredBoxState()

      val proposition: Propositions.Contextual.RequiredBoxState =
        Propositions.Contextual.RequiredBoxState(location, boxes)
    }
  }

  object Example {

    case class EnumeratedInput(inputs: List[Int], value: Int) extends Credential {
      override def prove(currentProof: Proof): Proof = Proofs.Example.EnumeratedInput(value)
      override def proposition: Proposition = Propositions.Example.EnumeratedInput(inputs)
    }
  }

  object Script {

    case class JS(proposition: Propositions.Script.JS, arguments: Json) extends Credential {
      def prove(currentProof: Proof): Proof = Proofs.Script.JS(arguments.toString())
    }
  }
}
