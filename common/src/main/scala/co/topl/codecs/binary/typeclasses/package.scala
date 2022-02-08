package co.topl.codecs.binary

package object typeclasses {

  trait Implicits
      extends BinaryShow.ToBinaryShowOps
      with Persistable.ToPersistableOps
      with Persistable.ToExtensionOps
      with Transmittable.ToTransmittableOps
      with Transmittable.ToExtensionOps

  object implicits extends Implicits
}
