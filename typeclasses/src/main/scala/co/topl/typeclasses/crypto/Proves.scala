package co.topl.typeclasses.crypto

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.{ProductPrivateKey, SymmetricKey}
import co.topl.models.Proofs.Consensus
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import com.google.common.primitives.Ints
import simulacrum.{op, typeclass}

import scala.language.implicitConversions

trait Proposes[T, Prop <: Proposition] {
  def propositionOf(t: T): Prop
}

object Proposes {

  trait Ops[T, Prop <: Proposition] {
    def t: T
    def typeclassInstance: Proposes[T, Prop]
    def proposition: Prop = typeclassInstance.propositionOf(t)
  }

  trait Implicits {

    implicit def asProposesOps[T, Prop <: Proposition](t1: T)(implicit ev: Proposes[T, Prop]): Ops[T, Prop] =
      new Ops[T, Prop] {
        def t: T = t1

        def typeclassInstance: Proposes[T, Prop] = ev
      }
  }

  object Implicits extends Implicits

  trait Instances {

    implicit val ed25519Proposes: Proposes[PublicKeys.Ed25519, Propositions.PublicKeyEd25519] =
      t => Propositions.PublicKeyEd25519(t)

    implicit val kesProposes: Proposes[PublicKeys.Kes, Propositions.Consensus.PublicKeyKes] =
      t => Propositions.Consensus.PublicKeyKes(t)
  }
}

trait Proves[T, Prf <: Proof] {
  def proveWith[Data: Signable](t: T, data: Data): Prf
}

object Proves {

  trait Instances {

    implicit val kesPrivateKeyProves: Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] =
      new Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] {

        def proveWith[Data: Signable](t: PrivateKeys.Kes, data: Data): Consensus.KesCertificate =
          Consensus.KesCertificate(
            Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))),
            Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))),
            Sized.strictUnsafe[Bytes, Consensus.KesCertificate.ChainCodeLength](Bytes(Array.fill[Byte](32)(0)))
          )
      }
  }
  object instances extends Instances
}

trait Verifiable[Prf <: Proof, Prop <: Proposition] {
  def verify[Data: Signable](proof: Prf, proposition: Prop, data: Data): Boolean
}

@typeclass trait Evolves[T] {
  @op("evolveSteps") def evolve(t: T, timesteps: Long): T
}

object Evolves {

  trait Instances {

    implicit val kesPrivateKeyEvolves: Evolves[PrivateKeys.Kes] = {
      val scheme = new KeyEvolvingSignatureScheme
      (key, timesteps) => {
        val symmetricKey =
          SymmetricKey(
            ProductPrivateKey.deserializeProductKey(
              Ints.toByteArray(
                key.bytes.data.toArray.length
              ) ++ key.bytes.data.toArray
            )
          )
        val updatedSymmetricKey =
          scheme.updateSymmetricProductKey(symmetricKey, timesteps.toInt) // TODO: toInt?
        PrivateKeys.Kes(
          Sized.strictUnsafe(Bytes(ProductPrivateKey.serializer.getBytes(updatedSymmetricKey)))
        )
      }
    }
  }

  object instances extends Instances
}
