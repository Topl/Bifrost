package co.topl.typeclasses

import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.{Ed25519, ExtendedEd25519}
import co.topl.models._

import scala.language.implicitConversions

@simulacrum.typeclass
trait Prover[ProofInput] {

  /**
   * Creates a Proof using this Prover's input type
   * @param t a value which can construct a Proof, usually a SecretKey
   * @return a Proof
   */
  @simulacrum.op("asProof")
  def proveWith(t: ProofInput): Proof
}

object Prover {

  trait Instances {

    implicit val curve25519Proves: Prover[(SecretKeys.Curve25519, Transaction.Unproven)] =
      (t: (SecretKeys.Curve25519, Transaction.Unproven)) => ??? // TODO
//        Curve25519.instance.sign(t._1, t._2.signableBytes)

    implicit def ed25519Proves(implicit
      ed: Ed25519
    ): Prover[(SecretKeys.Ed25519, Transaction.Unproven)] =
      (t: (SecretKeys.Ed25519, Transaction.Unproven)) => ed.sign(t._1, t._2.signableBytes)

    implicit def extendedEd25519Proves(implicit
      extendedEd: ExtendedEd25519
    ): Prover[(SecretKeys.ExtendedEd25519, Transaction.Unproven)] =
      (t: (SecretKeys.ExtendedEd25519, Transaction.Unproven)) => extendedEd.sign(t._1, t._2.signableBytes)

  }

  object instances extends Instances
}
