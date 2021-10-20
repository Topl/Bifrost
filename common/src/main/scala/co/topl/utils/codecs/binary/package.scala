package co.topl.utils.codecs

package object binary {

  trait Codecs extends valuetypes.Codecs with attestation.Codecs with modifier.Codecs with crypto.Codecs

  trait Implicits
      extends valuetypes.Instances
      with AsBytes.Instances
      with AsBytes.ToOps
      with FromBytes.Instances
      with FromBytes.ToOps
      with BytesExtensions
      with SizedBytesCodec.Instances
      with StringDataTypesCodec.Instances
      with crypto.Instances

  object codecs extends Codecs
  object implicits extends Implicits
}
