package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.models.SecretKeys
import simulacrum.{op, typeclass}

@typeclass trait Evolves[T] {
  @op("evolveSteps") def evolve(t: T, timesteps: Long): T
}

object Evolves {

  trait Instances {

    implicit val kesPrivateKeyEvolves: Evolves[SecretKeys.SymmetricMMM] = {
      val scheme = new KeyEvolvingSignatureScheme
      (key, timesteps) => scheme.updateSymmetricProductKey(key, timesteps.toInt) // TODO: toInt?
    }
  }

  object instances extends Instances
}
