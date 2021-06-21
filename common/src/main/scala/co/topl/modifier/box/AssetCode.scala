package co.topl.modifier.box

import co.topl.attestation.{Address, AddressSerializer}
import co.topl.modifier.box.AssetCode.AssetCodeVersion
import co.topl.utils.codecs.implicits._
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.utils.encode.Base58
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
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
  override def serializer: BifrostSerializer[AssetCode] = AssetCode

  override def toString: String = Base58.encode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case ec: AssetCode => bytes sameElements ec.bytes
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object AssetCode extends BifrostSerializer[AssetCode] {
  type AssetCodeVersion = Byte

  val shortNameLimit = 8 // limit to the asset shortName is 8 Latin-1 encoded characters

  implicit val jsonEncoder: Encoder[AssetCode] = (ac: AssetCode) => ac.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[AssetCode] = (ac: AssetCode) => ac.toString

  implicit val jsonDecoder: Decoder[AssetCode] = Decoder[Base58Data].map(fromBase58)
  implicit val jsonKeyDecoder: KeyDecoder[AssetCode] = KeyDecoder[Base58Data].map(fromBase58)

  private def fromBase58(data: Base58Data): AssetCode = parseBytes(data.value) match {
    case Success(ec)  => ec
    case Failure(err) => throw err
  }

  override def serialize(obj: AssetCode, w: Writer): Unit = {
    // should be safe to assume Latin-1 encoding since AssetCode already checks this once instantiation
    val paddedShortName = obj.shortName.value.padTo(shortNameLimit, 0: Byte)

    w.put(obj.version)
    AddressSerializer.serialize(obj.issuer, w)
    w.putBytes(paddedShortName)
  }

  override def parse(r: Reader): AssetCode =
    new AssetCode(
      r.getByte(),
      AddressSerializer.parse(r),
      r.getBytes(shortNameLimit).filter(_ != 0).infalliblyDecodeTo[Latin1Data]
    )
}
