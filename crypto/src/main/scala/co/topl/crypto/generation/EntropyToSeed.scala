package co.topl.crypto.generation

import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.Password
import co.topl.models.Bytes
import co.topl.models.utility.{Length, Sized}
import co.topl.models.utility.HasLength.instances._

import java.nio.charset.StandardCharsets

trait EntropyToSeed[SeedLength <: Length] {
  def toSeed(entropy: Entropy, password: Option[Password]): Sized.Strict[Bytes, SeedLength]
}

object EntropyToSeed {

  trait Instances {

    implicit def pbkdf2Sha512[SeedLength <: Length](implicit seedLength: SeedLength): EntropyToSeed[SeedLength] =
      (entropy: Entropy, password: Option[Password]) => {
        val kdf = new Pbkdf2Sha512()
        Sized.strictUnsafe(
          Bytes(
            kdf.generateKey(
              password.getOrElse("").getBytes(StandardCharsets.UTF_8),
              entropy.value,
              seedLength.value,
              4096
            )
          )
        )
      }

  }

  object instances extends Instances
}
