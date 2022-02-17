package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.models.utility.Ratio

trait TetraTransmittableCodecs {
  import TetraScodecCodecs._

  implicit val ratioTransmittable: Transmittable[Ratio] = Transmittable.instanceFromCodec
}

object TetraTransmittableCodecs extends TetraTransmittableCodecs
