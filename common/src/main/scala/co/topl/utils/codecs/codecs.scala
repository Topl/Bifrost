package co.topl.utils

package object codecs {

  trait Implicits
      extends AsBytes.Instances
      with AsBytes.ToOps
      with FromBytes.Instances
      with FromBytes.ToOps
      with CryptoCodec.CryptoCodecInstances
      with StringDataTypesCodec.StringTypesInstances
      with SizedBytesCodec.Instances

  object implicits extends Implicits

}
