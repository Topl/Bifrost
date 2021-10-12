package co.topl.crypto.typeclasses

import co.topl.models.SecretKeys
import simulacrum.{op, typeclass}

@typeclass trait Evolves[T] {
  @op("evolveSteps") def evolve(t: T, timesteps: Long): T
}

object Evolves {

  trait Instances {

    implicit val kesSumSkEvolve: Evolves[SecretKeys.KesSum] = ???
  }

  object instances extends Instances
}
