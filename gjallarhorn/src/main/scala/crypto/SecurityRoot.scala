package crypto

import co.topl.crypto.Implicits.bytesOfArrayBytes
import co.topl.crypto.hash.Digest32
import co.topl.crypto.utils.Base58
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import utils.serialization.{BytesSerializable, GjalSerializer, Reader, Writer}

import scala.util.{Failure, Success, Try}

/**
 * SecurityRoot is currently used for AssetValues.
 * A cryptographic commitment to an accumulator that is used for membership proofs
 * @param root - bytes used to create a security root
 */
class SecurityRoot private (private val root: Array[Byte]) extends BytesSerializable {

  require(root.length == SecurityRoot.size, "Invalid securityRoot")

  type M = SecurityRoot
  lazy val serializer: GjalSerializer[SecurityRoot] = SecurityRoot

  def getRoot: Array[Byte] = root

  override def hashCode(): Int = Ints.fromByteArray(root)

  override def equals(obj: Any): Boolean = obj match {
    case sr: SecurityRoot => sr.root sameElements root
    case _                => false
  }

  override def toString: String = Base58.encode(root)
}

object SecurityRoot extends GjalSerializer[SecurityRoot] {

  val size: Int = Digest32.size // 32 bytes
  val empty: SecurityRoot = new SecurityRoot(Array.fill(size)(0: Byte))

  implicit val jsonEncoder: Encoder[SecurityRoot] = (sr: SecurityRoot) => sr.toString.asJson
  implicit val jsonDecoder: Decoder[SecurityRoot] = Decoder.decodeString.emapTry(sr => Try(SecurityRoot(sr)))

  def apply(str: String): SecurityRoot = Base58.decode(str) match {
    case Success(value)     => new SecurityRoot(value)
    case Failure(exception) => throw new Exception("Unable to decode SecurityRoot")
  }

  override def serialize(obj: SecurityRoot, w: Writer): Unit =
    w.putBytes(obj.root)

  override def parse(r: Reader): SecurityRoot = {
    val root: Array[Byte] = r.getBytes(size)
    new SecurityRoot(root)
  }
}
