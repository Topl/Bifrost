package co.topl.codecs.binary.scodecs.modifier.box

import cats.implicits._
import co.topl.codecs.binary.scodecs.attestation._
import co.topl.codecs.binary.scodecs.crypto._
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.modifier.box._
import co.topl.utils.StringDataTypes.Latin1Data
import scodec.{Codec, Err}
import scodec.codecs.discriminated
import co.topl.codecs.binary.scodecs.ops.implicits._

trait BoxCodecs {

  implicit val securityRootCodec: Codec[SecurityRoot] =
    bytesCodec(SecurityRoot.size)
      .mapDecodeErr(_ => Err(s"security root must be ${SecurityRoot.size} bytes long"))
      .as[SecurityRoot]

  implicit val boxIdCodec: Codec[BoxId] = digest32Codec.as[BoxId]

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
