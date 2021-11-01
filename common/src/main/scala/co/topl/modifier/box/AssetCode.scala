package co.topl.modifier.box

import co.topl.attestation.Address
import co.topl.modifier.box.AssetCode.AssetCodeVersion
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.utils.codecs.binary.legacy.modifier.box.AssetCodeSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.utils.codecs.json.codecs._
import co.topl.utils.encode.Base58
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.util.{Failure, Success}

/**
 * AssetCode serves as a unique identifier for user issued assets
 */
case class AssetCode(version: AssetCodeVersion, issuer: Address, shortName: Latin1Data) extends BytesSerializable {

  require(version == 1.toByte, "AssetCode version required to be 1")

  require(
    shortName.value.length <= AssetCode.shortNameLimit,
    "Asset short names must be less than 8 Latin-1 encoded characters"
  )

  override type M = AssetCode
  override def serializer: BifrostSerializer[AssetCode] = AssetCodeSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case ec: AssetCode => bytes sameElements ec.bytes
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object AssetCode {
  type AssetCodeVersion = Byte

  val shortNameLimit = 8 // limit to the asset shortName is 8 Latin-1 encoded characters

  implicit val jsonEncoder: Encoder[AssetCode] = (ac: AssetCode) => ac.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[AssetCode] = (ac: AssetCode) => ac.toString

  implicit val jsonDecoder: Decoder[AssetCode] = Decoder[Base58Data].map(fromBase58)
  implicit val jsonKeyDecoder: KeyDecoder[AssetCode] = KeyDecoder[Base58Data].map(fromBase58)

  private def fromBase58(data: Base58Data): AssetCode = AssetCodeSerializer.parseBytes(data.value) match {
    case Success(ec)  => ec
    case Failure(err) => throw err
  }
}
