package co.topl.codecs.bytes

package object typeclasses {

  trait Implicits
      extends BinaryShow.ToBinaryShowOps
      with Persistable.ToPersistableOps
      with Persistable.ToExtensionOps
      with Transmittable.ToTransmittableOps
      with Transmittable.ToExtensionOps
      with ImmutableCodec.ToImmutableCodecOps
      with ImmutableEncoder.ToImmutableEncoderOps
      with ImmutableDecoder.ToImmutableDecoderOps
      with ImmutableDecoder.ToExtensionOps
      with Identifiable.ToIdentifiableOps
      with Signable.ToSignableOps

  object implicits extends Implicits
}
