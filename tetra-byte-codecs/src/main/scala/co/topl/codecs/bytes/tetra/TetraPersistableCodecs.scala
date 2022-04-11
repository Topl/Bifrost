package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.{Persistable, Transmittable}
import co.topl.models.SecretKeys

trait TetraPersistableCodecs {
  import TetraScodecCodecs._

  implicit val persistableCurve25519SecretKey: Persistable[SecretKeys.Curve25519] = Persistable.instanceFromCodec
  implicit val persistableEd25519SecretKey: Persistable[SecretKeys.Ed25519] = Persistable.instanceFromCodec

  implicit val persistableExtendedEd25519SecretKey: Persistable[SecretKeys.ExtendedEd25519] =
    Persistable.instanceFromCodec

  implicit val persistableKesProductSecretKey: Persistable[SecretKeys.KesProduct] =
    Persistable.instanceFromCodec
}

object TetraPersistableCodecs extends TetraPersistableCodecs
