package co.topl.nodeView.state

import co.topl.crypto.FastCryptographicHash
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}

case class ProgramId (hashBytes: Array[Byte]) {

  require(hashBytes.length == ProgramId.size, s"Invalid size for ProgramId")

  override def hashCode: Int = Ints.fromByteArray(hashBytes)

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[ProgramId] &&
      java.util.Arrays.equals(hashBytes, o.asInstanceOf[ProgramId].hashBytes)
  }

  override def toString: String = Base58.encode(hashBytes)
}


object ProgramId extends BifrostSerializer[ProgramId] {

  val size: Int = FastCryptographicHash.DigestSize; // number of bytes in identifier,

  def apply(id: String): ProgramId = {
    Base58.decode(id) match {
      case Success(id) => new ProgramId(id)
      case Failure(ex) => throw ex
    }
  }

  def create (seed: Array[Byte]): ProgramId = {
    new ProgramId(FastCryptographicHash(seed))
  }

  override def serialize(obj: ProgramId, w: Writer): Unit = {
    w.putBytes(obj.hashBytes)
  }

  override def parse(r: Reader): ProgramId = {
    ProgramId(r.getBytes(size))
  }

  implicit val jsonEncoder: Encoder[ProgramId] =
    (id: ProgramId) => id.toString.asJson

  implicit val jsonDecoder: Decoder[ProgramId] =
    Decoder.decodeString.emapTry { id => Try(ProgramId(id)) }

  implicit val jsonKeyEncoder: KeyEncoder[ProgramId] =
    ( id: ProgramId ) => id.toString

  implicit val jsonKeyDecoder: KeyDecoder[ProgramId] =
    ( id: String ) => Some(ProgramId(id))
}
