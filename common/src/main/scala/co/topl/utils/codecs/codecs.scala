package co.topl.utils

package object codecs {

  object implicits
      extends AsBytes.Instances
      with AsBytes.ToOps
      with FromBytes.Instances
      with FromBytes.ToOps
      with CryptoCodec.FromBytesInstances
      with CryptoCodec.AsBytesInstances
      with CryptoCodec.JsonDecoderInstances
      with CryptoCodec.JsonEncoderInstances
}
