package co.topl.modifier.box

import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import co.topl.crypto.hash.Hash
import co.topl.utils.encode.Base58

import scala.util.{Failure, Success}

case class ProgramId (private val hashBytes: Array[Byte]) extends BytesSerializable {

  override type M = ProgramId
  override def serializer: BifrostSerializer[ProgramId] = ProgramId

  override def equals(obj: Any): Boolean = obj match {
    case obj: ProgramId => obj.hashBytes sameElements hashBytes
    case _ => false
  }

  override def toString: String = Base58.encode(hashBytes)

  override def hashCode: Int = Ints.fromByteArray(hashBytes)
}


object ProgramId extends BifrostSerializer[ProgramId] {

  // use Blake2b256 hashing
  import co.topl.crypto.hash.Blake2b256._

  val size: Int = Hash.digestSize; // number of bytes in identifier,

  def apply(id: String): ProgramId = {
    Base58.decode(id) match {
      case Success(id) =>
        require(id.length == ProgramId.size, s"Invalid size for ProgramId")
        new ProgramId(id)

      case Failure(ex) => throw ex
    }
  }

  def create (seed: Array[Byte]): ProgramId = {
    new ProgramId(Hash(seed).toBytes)
  }

  override def serialize(obj: ProgramId, w: Writer): Unit = {
    w.putBytes(obj.hashBytes)
  }

  override def parse(r: Reader): ProgramId = {
    ProgramId(r.getBytes(size))
  }

  implicit val jsonEncoder: Encoder[ProgramId] = (id: ProgramId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ProgramId] = ( id: ProgramId ) => id.toString
  implicit val jsonDecoder: Decoder[ProgramId] = Decoder.decodeString.map(apply)
  implicit val jsonKeyDecoder: KeyDecoder[ProgramId] = ( id: String ) => Some(apply(id))
}
