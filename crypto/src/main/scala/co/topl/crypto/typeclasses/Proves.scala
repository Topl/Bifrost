package co.topl.crypto.typeclasses

import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

import scala.language.implicitConversions

trait Proves[T, Prf <: Proof] {
  def proveWith[Data: Signable](t: T, data: Data): Prf
}

object Proves {

  trait Instances {

    implicit val kesPrivateKeyProves: Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] =
      new Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] {

        def proveWith[Data: Signable](t: PrivateKeys.Kes, data: Data): Proofs.Consensus.KesCertificate =
          Proofs.Consensus.KesCertificate(
            Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))),
            Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))),
            Sized.strictUnsafe(
              Bytes(Array.fill[Byte](implicitly[Proofs.Consensus.KesCertificate.ChainCodeLength].value)(0))
            )
          )
      }
  }
  object instances extends Instances
}
