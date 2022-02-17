package co.topl.codecs.binary

import co.topl.codecs.binary.typeclasses.{BinaryShowInstances, PersistableInstances, TransmittableInstances}

trait BinaryCodecs
    extends scodecs.ScodecImplicits
    with typeclasses.Persistable.ToPersistableOps
    with typeclasses.Persistable.ToExtensionOps
    with typeclasses.Transmittable.ToTransmittableOps
    with typeclasses.Transmittable.ToExtensionOps
    with typeclasses.BinaryShow.ToBinaryShowOps
    with TransmittableInstances
    with PersistableInstances
    with BinaryShowInstances
