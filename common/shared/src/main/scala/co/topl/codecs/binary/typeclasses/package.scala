package co.topl.codecs.binary

package object typeclasses {

  trait Implicits
      extends BinaryShow.ToBinaryShowOps
      with BinaryShowInstances
      with Persistable.ToPersistableOps
      with Persistable.ToExtensionOps
      with PersistableInstances
      with Transmittable.ToTransmittableOps
      with Transmittable.ToExtensionOps
      with TransmittableInstances

  object implicits extends Implicits
}
