package bifrost.nodeView

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.network.message.InvData
import bifrost.nodeView.NodeViewModifier.ModifierId
import bifrost.serialization.{BytesSerializable, JsonSerializable}
import bifrost.state.MinimalState
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.BifrostEncoder
import bifrost.utils.encode.Base16
import com.typesafe.config.ConfigFactory
import scorex.crypto.encode.Base58
import supertagged.TaggedType

import scala.util.Try

trait NodeViewModifier extends BytesSerializable with JsonSerializable {
  self =>

  import NodeViewModifier.{ModifierId, ModifierTypeId}

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId
}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  val DefaultIdSize = 32 // in bytes

  //TODO implement ModifierTypeId as a trait
  object ModifierTypeId extends TaggedType[Byte]
  type ModifierTypeId = ModifierTypeId.Type

  object ModifierId extends TaggedType[String]
  type ModifierId = ModifierId.Type

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)])(implicit enc: BifrostEncoder): String = {
    List(ids.headOption, ids.lastOption)
      .flatten
      .map { case (typeId, id) => s"($typeId,${enc.encodeId(id)})" }
      .mkString("[", "..", "]")
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId])(implicit encoder: BifrostEncoder): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData)(implicit encoder: BifrostEncoder): String = idsToString(invData.typeId, invData.ids)

  def bytesToId: Array[Byte] => ModifierId = bifrost.utils.bytesToId

  def idToBytes: ModifierId => Array[Byte] = bifrost.utils.idToBytes
}



trait PersistentNodeViewModifier[P <: Proposition, TX <: GenericTransaction[P]] extends NodeViewModifier {

  def parentId: ModifierId

  // with Dotty is would be Seq[TX] | Nothing
  def transactions: Option[Seq[TX]]
}