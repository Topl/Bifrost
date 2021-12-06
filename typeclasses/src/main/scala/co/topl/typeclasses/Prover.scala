package co.topl.typeclasses

import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.language.implicitConversions

trait Prover[ProofInput, Prf <: Proof] {

  /**
   * Creates a Proof using this Prover's input type
   * @param t a value which can construct a Proof, usually a SecretKey
   * @return a Proof
   */
  def proveWith(t: ProofInput): Prf
}

object Prover {

  def apply[T, Prf <: Proof](implicit p: Prover[T, Prf]): Prover[T, Prf] = p

  trait Instances {

    implicit def curve25519Proves[Data: Signable]: Prover[(SecretKeys.Curve25519, Data), Proofs.Knowledge.Curve25519] =
      (t: (SecretKeys.Curve25519, Data)) => new Curve25519().sign(t._1, t._2.signableBytes)

    implicit def ed25519Proves[Data: Signable](implicit
      ed: Ed25519
    ): Prover[(SecretKeys.Ed25519, Data), Proofs.Knowledge.Ed25519] =
      (t: (SecretKeys.Ed25519, Data)) => ed.sign(t._1, t._2.signableBytes)

    implicit def extendedEd25519Proves[Data: Signable](implicit
      extendedEd: ExtendedEd25519
    ): Prover[(SecretKeys.ExtendedEd25519, Data), Proofs.Knowledge.Ed25519] =
      (t: (SecretKeys.ExtendedEd25519, Data)) => extendedEd.sign(t._1, t._2.signableBytes)

    implicit val blockProvesHeightLock: Prover[BlockHeaderV2, Proofs.Contextual.HeightLock] =
      (t: BlockHeaderV2) => Proofs.Contextual.HeightLock(t.id)
  }

  trait Implicits {

    implicit class TOps[T](private val t: T) {

      def prove[Prf <: Proof](implicit proves: Prover[T, Prf]): Prf =
        proves.proveWith(t)
    }
  }

  object implicits extends Implicits

  object instances extends Instances
}
