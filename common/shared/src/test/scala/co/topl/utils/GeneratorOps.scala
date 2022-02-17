package co.topl.utils

import org.scalacheck.Gen
import org.scalacheck.rng.Seed

object GeneratorOps {

  implicit class GeneratorOps[T](gen: Gen[T]) {

    def sampleFirst(
      parameters: Gen.Parameters = Gen.Parameters.default,
      seed:       Seed = Seed.random(),
      retries:    Int = 100
    ): T =
      gen.pureApply(parameters, seed, retries)
  }
}
