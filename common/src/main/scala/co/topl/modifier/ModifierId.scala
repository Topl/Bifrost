package co.topl.modifier

import co.topl.crypto.BytesOf
import co.topl.crypto.Implicits._
import co.topl.crypto.hash.Digest
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.utils.{blake2b256, HashDigest}
import co.topl.utils.encode.Base58
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import scala.util.{Failure, Success, Try}

class ModifierId private (private val value: Array[Byte]) extends BytesSerializable {

  require(value.length == ModifierId.size, s"Invalid size for ModifierId")

  type M = ModifierId
  lazy val serializer: BifrostSerializer[ModifierId] = ModifierId

  def getIdBytes: Array[Byte] = value.tail
  def getModType: ModifierTypeId = ModifierTypeId(value.head)

  override def hashCode: Int = Ints.fromByteArray(value)

  override def equals(obj: Any): Boolean = obj match {
    case mId: ModifierId => mId.value sameElements value
    case _               => false
  }

  override def toString: String = Base58.encode(value)
}

object ModifierId extends BifrostSerializer[ModifierId] {

  val size: Int = 1 + Digest[HashDigest].size // ModifierId's are derived from Blake2b-256
  val empty: ModifierId = new ModifierId(Array.fill(size)(0: Byte))

  val genesisParentId: ModifierId =
    new ModifierId(Block.modifierTypeId.value +: Array.fill(Digest[HashDigest].size)(1: Byte))

  implicit val ord: Ordering[ModifierId] = Ordering.by(_.toString)

  implicit val jsonEncoder: Encoder[ModifierId] = (id: ModifierId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ModifierId] = (id: ModifierId) => id.toString
  implicit val jsonDecoder: Decoder[ModifierId] = Decoder.decodeString.emapTry(id => Try(ModifierId(id)))
  implicit val jsonKeyDecoder: KeyDecoder[ModifierId] = (id: String) => Some(ModifierId(id))

  def apply(nodeViewModifier: NodeViewModifier): ModifierId = nodeViewModifier match {
    case mod: Block =>
      new ModifierId(BytesOf[HashDigest].prepend(blake2b256(mod.messageToSign), Block.modifierTypeId.value))
    case mod: Transaction.TX =>
      new ModifierId(BytesOf[HashDigest].prepend(blake2b256(mod.messageToSign), Transaction.modifierTypeId.value))
    case _ => throw new Error("Only blocks and transactions generate a modifierId")
  }

  def apply(str: String): ModifierId =
    Base58.decode(str) match {
      case Success(id) => new ModifierId(id)
      case Failure(ex) => throw ex
    }

  def serialize(obj: ModifierId, w: Writer): Unit =
    /* value: Array[Byte] */
    w.putBytes(obj.value)

  def parse(r: Reader): ModifierId = {
    val value: Array[Byte] = r.getBytes(size)
    new ModifierId(value)
  }

}
