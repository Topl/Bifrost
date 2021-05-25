package co.topl.modifier.box

import cats.implicits.toShow
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.utils.StringTypes.implicits._
import co.topl.utils.StringTypes.Base58String
import co.topl.utils.encode.Base58
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class ProgramId(private val hashBytes: Array[Byte]) extends BytesSerializable {

  override type M = ProgramId
  override def serializer: BifrostSerializer[ProgramId] = ProgramId

  override def equals(obj: Any): Boolean = obj match {
    case obj: ProgramId => obj.hashBytes sameElements hashBytes
    case _              => false
  }

  override def toString: String = Base58.encode(hashBytes).show

  override def hashCode: Int = Ints.fromByteArray(hashBytes)
}

object ProgramId extends BifrostSerializer[ProgramId] {

  val size: Int = Digest32.size; // number of bytes in identifier,

  def apply(id: Base58String): ProgramId = {
    val idBytes = Base58.decode(id)

    require(idBytes.length == ProgramId.size, s"Invalid size for ProgramId")

    new ProgramId(idBytes)
  }

  def create(seed: Array[Byte]): ProgramId =
    new ProgramId(blake2b256.hash(seed).value)

  override def serialize(obj: ProgramId, w: Writer): Unit =
    w.putBytes(obj.hashBytes)

  override def parse(r: Reader): ProgramId =
    ProgramId(r.getBytes(size))

  implicit val jsonEncoder: Encoder[ProgramId] = (id: ProgramId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ProgramId] = (id: ProgramId) => id.toString

  implicit val jsonDecoder: Decoder[ProgramId] =
    Decoder.decodeString.emap(Base58String.validated(_).leftMap(_ => "Value is not Base 58").toEither).map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[ProgramId] = (id: String) => Base58String.validated(id).map(apply).toOption
}
