package co.topl.modifier

import cats.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.digest.implicits._
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.utils.codecs.binary.implicits._
import co.topl.utils.codecs.json.codecs._
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

case class ModifierId(value: Array[Byte]) extends BytesSerializable {

  require(value.length == ModifierId.size, s"Invalid size for ModifierId")

  type M = ModifierId
  lazy val serializer: BifrostSerializer[ModifierId] = ModifierIdSerializer

  def getIdBytes: Array[Byte] = value.tail
  def getModType: ModifierTypeId = ModifierTypeId(value.head)

  override def hashCode: Int = Ints.fromByteArray(value)

  override def equals(obj: Any): Boolean = obj match {
    case mId: ModifierId => mId.value sameElements value
    case _               => false
  }

  override def toString: String = value.encodeAsBase58.show
}

object ModifierId {

  val size: Int = 1 + Digest32.size // ModifierId's are derived from Blake2b-256
  val empty: ModifierId = new ModifierId(Array.fill(size)(0: Byte))

  val genesisParentId: ModifierId =
    new ModifierId(Block.modifierTypeId.value +: Array.fill(Digest32.size)(1: Byte))

  implicit val ord: Ordering[ModifierId] = Ordering.by(_.toString)

  sealed trait CreateModifierIdFailure
  case object InvalidModifierFailure extends CreateModifierIdFailure

  implicit val jsonEncoder: Encoder[ModifierId] = (id: ModifierId) => id.toString.asJson
  implicit val jsonKeyEncoder: KeyEncoder[ModifierId] = (id: ModifierId) => id.toString

  implicit val jsonDecoder: Decoder[ModifierId] =
    Decoder[Base58Data]
      .ensure(_.value.length == ModifierId.size, "Invalid size for ModifierId")
      .map(ModifierId.fromBase58)
  implicit val jsonKeyDecoder: KeyDecoder[ModifierId] = KeyDecoder[Base58Data].map(ModifierId.fromBase58)

  /**
   * Creates a modifier ID from a node view modifier.
   * @param nodeViewModifier the modifier to create an ID from
   * @return a create result with a CreateModifierIdFailure if a failure occurred
   */
  def create(nodeViewModifier: NodeViewModifier): Either[CreateModifierIdFailure, ModifierId] = nodeViewModifier match {
    case mod: Block =>
      new ModifierId(
        Block.modifierTypeId.value +: blake2b256.hash(mod.messageToSign).bytes
      ).asRight[CreateModifierIdFailure]
    case mod: Transaction.TX =>
      new ModifierId(Transaction.modifierTypeId.value +: blake2b256.hash(mod.messageToSign).bytes)
        .asRight[CreateModifierIdFailure]
    case _ => Left(InvalidModifierFailure)
  }

  def apply(nodeViewModifier: NodeViewModifier): ModifierId = nodeViewModifier match {
    case mod: Block =>
      new ModifierId(Block.modifierTypeId.value +: blake2b256.hash(mod.messageToSign).bytes)
    case mod: Transaction.TX =>
      new ModifierId(Transaction.modifierTypeId.value +: blake2b256.hash(mod.messageToSign).bytes)
    case _ => throw new Error("Only blocks and transactions generate a modifierId")
  }

  @deprecated
  def apply(str: String): ModifierId = new ModifierId(Base58Data.unsafe(str).value)

  /**
   * Creates a modifier ID from a base 58 encoded data.
   * @param data the string to turn into a modifier ID
   * @return modifier id
   */
  def fromBase58(data: Base58Data): ModifierId = new ModifierId(data.value)

}
