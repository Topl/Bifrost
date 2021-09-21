package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.{ProductPrivateKey, SymmetricKey}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, PrivateKeys}
import com.google.common.primitives.Ints
import simulacrum.{op, typeclass}

@typeclass trait Evolves[T] {
  @op("evolveSteps") def evolve(t: T, timesteps: Long): T
}

object Evolves {

  trait Instances {

    implicit val kesPrivateKeyEvolves: Evolves[PrivateKeys.Kes] = {
      val scheme = new KeyEvolvingSignatureScheme
      (key, timesteps) => {
        val symmetricKey = SymmetricKey.deserializeSymmetricKey(key.bytes.data.toArray)
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
