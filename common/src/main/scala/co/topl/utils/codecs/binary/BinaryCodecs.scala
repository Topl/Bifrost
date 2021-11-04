package co.topl.utils.codecs.binary

trait BinaryCodecs
    extends scodecs.ScodecImplicits
    with typeclasses.Persistable.ToPersistableOps
    with typeclasses.Persistable.ToExtensionOps
    with typeclasses.Transmittable.ToTransmittableOps
    with typeclasses.Transmittable.ToExtensionOps
    with typeclasses.BinaryShow.ToBinaryShowOps
    with network.TransmittableInstances
    with persistence.PersistableInstances
    with show.BinaryShowInstances
