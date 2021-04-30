package crypto

import attestation.Address
import co.topl.utils.BytesOf.Implicits._
import co.topl.utils.encode.Base58
import com.google.common.primitives.Ints
import crypto.AssetCode.AssetCodeVersion
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import utils.serialization.{BytesSerializable, GjalSerializer, Reader, Writer}

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success}

/**
 * AssetCode serves as a unique identifier for user issued assets
 * @param version single byte for asset code version
 * @param issuer the address for the issuer of the asset
 * @param shortName string used to create the asset code
 */
case class AssetCode private (version: AssetCodeVersion, issuer: Address, shortName: String) extends BytesSerializable {

  require(
    shortName.getBytes(StandardCharsets.UTF_8).length <= AssetCode.shortNameLimit,
    "Asset short names must be less than 8 UTF-8 encoded characters"
  )

  override type M = AssetCode
  override def serializer: GjalSerializer[AssetCode] = AssetCode

  override def toString: String = Base58.encode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case ec: AssetCode => bytes sameElements ec.bytes
    case _             => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object AssetCode extends GjalSerializer[AssetCode] {
  type AssetCodeVersion = Byte

  val shortNameLimit = 8 // limit to the asset shortName is 8 UTF-8 encoded characters

  implicit val jsonEncoder: Encoder[AssetCode] = (ac: AssetCode) => ac.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[AssetCode] = (ac: AssetCode) => ac.toString
  implicit val jsonDecoder: Decoder[AssetCode] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[AssetCode] = (str: String) => Some(apply(str))

  private def apply(str: String): AssetCode =
    Base58.decode(str).flatMap(parseBytes) match {
      case Success(ec) => ec
      case Failure(ex) => throw ex
    }

  override def serialize(obj: AssetCode, w: Writer): Unit = {
    w.put(obj.version)
    Address.serialize(obj.issuer, w)
    w.putByteString(obj.shortName)
  }

  override def parse(r: Reader): AssetCode = {
    val version = r.getByte()
    val issuer = Address.parse(r)
    val shortName = r.getByteString()
    require(version == 1.toByte, "AssetCode version required to be 1")
    new AssetCode(version, issuer, shortName)
  }
}
