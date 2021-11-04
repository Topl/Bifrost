package co.topl.modifier.box

import cats.implicits._
import co.topl.crypto.hash.digest.Digest32
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import co.topl.utils.codecs.binary.legacy.modifier.box.SecurityRootSerializer
import co.topl.utils.codecs._
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import co.topl.utils.catsInstances._

case class SecurityRoot(root: Array[Byte]) {

  require(root.length == SecurityRoot.size, "Invalid securityRoot")

  lazy val serializer: BifrostSerializer[SecurityRoot] = SecurityRootSerializer

  def getRoot: Array[Byte] = root

  override def hashCode(): Int = Ints.fromByteArray(root)

  override def equals(obj: Any): Boolean = obj match {
    case sr: SecurityRoot => sr.root sameElements root
    case _                => false
  }

  override def toString: String = root.show
}

object SecurityRoot {

  val size: Int = Digest32.size // 32 bytes
  val empty: SecurityRoot = new SecurityRoot(Array.fill(size)(0: Byte))

  implicit val jsonEncoder: Encoder[SecurityRoot] = _.transmittableBase58.asJson
  implicit val jsonDecoder: Decoder[SecurityRoot] = Decoder[Base58Data].emap(_.value.decodeTransmitted[SecurityRoot])
}
