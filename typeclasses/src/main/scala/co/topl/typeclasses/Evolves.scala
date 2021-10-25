package co.topl.typeclasses

import co.topl.crypto.mnemonic.{Bip32Index, Bip32Indexes}
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.models.{SecretKeys, VerificationKeys}
import simulacrum.{op, typeclass}

@typeclass trait Evolves[T] {
  @op("evolveSteps") def evolve(t: T, index: Long): T
}

object Evolves {

  trait Instances {

    // should we make a new instance or use the global one?
    implicit val extendedEd25519SkEvolve: Evolves[SecretKeys.ExtendedEd25519] =
      (parent: SecretKeys.ExtendedEd25519, index: Long) => new ExtendedEd25519().deriveSecret(parent, Bip32Index(index))

    implicit val extendedEd25519VkEvolve: Evolves[VerificationKeys.ExtendedEd25519] =
      (parent: VerificationKeys.ExtendedEd25519, index: Long) =>
        new ExtendedEd25519().deriveVerification(parent, Bip32Indexes.SoftIndex(index.toInt))
  }

  object instances extends Instances
}
