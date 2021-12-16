package co.topl.typeclasses

import co.topl.credential.Credential
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models.{Propositions, SecretKeys, Transaction}

import scala.language.implicitConversions

/**
 * Reveal inputs to produce a Credential
 */
@simulacrum.typeclass
trait Credentialer[CredentialInput] {

  /**
   * Creates a Credential from the given T
   * @param t a value which can be converted into a Proposition (usually a Verification Key)
   * @return a Credential
   */
  @simulacrum.op("toCredential")
  def credentialOf(t: CredentialInput): Credential
}

object Credentialer {

  trait Implicits {

    implicit class IterableOps(creds: Iterable[Credential]) {

      def threshold(proposition: Propositions.Compositional.Threshold): Credential.Compositional.Threshold =
        Credential.Compositional.Threshold(proposition, creds)

      def and(proposition: Propositions.Compositional.And): Credential.Compositional.And =
        Credential.Compositional.And(proposition, creds)

      def Or(proposition: Propositions.Compositional.Or): Credential.Compositional.Or =
        Credential.Compositional.Or(proposition, creds)
    }
  }

  object implicits extends Implicits

  trait Instances {

    implicit val curve25519Credentialer: Credentialer[(SecretKeys.Curve25519, Transaction.Unproven)] =
      (t: (SecretKeys.Curve25519, Transaction.Unproven)) => Credential.Knowledge.Curve25519(t._1, t._2)

    implicit def ed25519Credentialer(implicit ed: Ed25519): Credentialer[(SecretKeys.Ed25519, Transaction.Unproven)] =
      (t: (SecretKeys.Ed25519, Transaction.Unproven)) => Credential.Knowledge.Ed25519(t._1, t._2)

    implicit def extendedEd25519Credentialer(implicit
      extendedEd: ExtendedEd25519
    ): Credentialer[(SecretKeys.ExtendedEd25519, Transaction.Unproven)] =
      (t: (SecretKeys.ExtendedEd25519, Transaction.Unproven)) => Credential.Knowledge.ExtendedEd25519(t._1, t._2)

    implicit val heightCredentialer: Credentialer[Propositions.Contextual.HeightLock] =
      t => Credential.Contextual.HeightLock(t.height)

    implicit val thresholdCredentialer: Credentialer[(Propositions.Compositional.Threshold, Iterable[Credential])] =
      (t: (Propositions.Compositional.Threshold, Iterable[Credential])) => Credential.Compositional.Threshold(t._1, t._2)

    implicit val AndCredentialer: Credentialer[(Propositions.Compositional.And, Iterable[Credential])] =
      (t: (Propositions.Compositional.And, Iterable[Credential])) => Credential.Compositional.And(t._1, t._2)

    implicit val OrCredentialer: Credentialer[(Propositions.Compositional.Or, Iterable[Credential])] =
      (t: (Propositions.Compositional.Or, Iterable[Credential])) => Credential.Compositional.Or(t._1, t._2)

    }

  object instances extends Instances
}
