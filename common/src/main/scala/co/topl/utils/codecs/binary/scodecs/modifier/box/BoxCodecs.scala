package co.topl.utils.codecs.binary.scodecs.modifier.box

import co.topl.modifier.box._
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.binary.scodecs.attestation._
import co.topl.utils.codecs.binary.scodecs.valuetypes._
import scodec.Codec
import scodec.codecs.discriminated

trait BoxCodecs {
  implicit val securityRootCodec: Codec[SecurityRoot] = bytesCodec(SecurityRoot.size).as[SecurityRoot]

  implicit val assetCodeCodec: Codec[AssetCode] =
    (byteCodec ::
      addressCodec ::
      bytesCodec(AssetCode.shortNameLimit)
        .xmap[Latin1Data](
          bytes => Latin1Data.fromData(bytes.filter(_ != 0)),
          _.value.padTo(AssetCode.shortNameLimit, 0: Byte)
        ))
      .as[AssetCode]

  implicit val simpleValueCodec: Codec[SimpleValue] = int128Codec.as[SimpleValue]

  implicit val assetValueCodec: Codec[AssetValue] =
    (int128Codec :: assetCodeCodec :: securityRootCodec :: optionCodec(latin1DataCodec)).as[AssetValue]

  implicit val tokenValueHolderCodec: Codec[TokenValueHolder] =
    discriminated[TokenValueHolder]
      .by(byteCodec)
      .typecase(SimpleValue.valueTypePrefix, simpleValueCodec)
      .typecase(AssetValue.valueTypePrefix, assetValueCodec)

  implicit val polyBoxCodec: Codec[PolyBox] =
    (evidenceCodec :: longCodec :: simpleValueCodec).as[PolyBox]

  implicit val arbitBoxCodec: Codec[ArbitBox] =
    (evidenceCodec :: longCodec :: simpleValueCodec).as[ArbitBox]

  implicit val assetBoxCodec: Codec[AssetBox] =
    (evidenceCodec :: longCodec :: assetValueCodec).as[AssetBox]

  implicit val boxCodec: Codec[Box[_]] =
    discriminated[Box[_]]
      .by(byteCodec)
      .typecase(PolyBox.typePrefix, polyBoxCodec)
      .typecase(ArbitBox.typePrefix, arbitBoxCodec)
      .typecase(AssetBox.typePrefix, assetBoxCodec)
}
