package co.topl.utils.codecs.binary

import co.topl.utils.StringDataTypes.{Base16Data, Base58Data, DataEncodingValidationFailure, Latin1Data}

object StringDataTypesCodec {

  trait Instances {

    implicit val base16DataAsBytes: AsBytes[Infallible, Base16Data] = AsBytes.infallible(_.value)

    implicit val base58DataAsBytes: AsBytes[Infallible, Base58Data] = AsBytes.infallible(_.value)

    implicit val latin1DataAsBytes: AsBytes[Infallible, Latin1Data] = AsBytes.infallible(_.value)

    implicit val base16DataFromBytes: FromBytes[Infallible, Base16Data] = FromBytes.infallible(Base16Data.fromData)

    implicit val base58DataFromBytes: FromBytes[Infallible, Base58Data] = FromBytes.infallible(Base58Data.fromData)

    implicit val latin1DataFromBytes: FromBytes[Infallible, Latin1Data] = FromBytes.infallible(Latin1Data.fromData)
  }

  object implicits extends Instances
}
