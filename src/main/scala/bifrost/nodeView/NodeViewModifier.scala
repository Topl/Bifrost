package bifrost.nodeView

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.nodeView.NodeViewModifier.ModifierId
import bifrost.serialization.{BytesSerializable, JsonSerializable}
import com.typesafe.config.ConfigFactory
import scorex.crypto.encode.Base58

import scala.util.Try

trait NodeViewModifier extends BytesSerializable with JsonSerializable {
  self =>

  import NodeViewModifier.{ModifierId, ModifierTypeId}

  val modifierTypeId: ModifierTypeId

  //todo: check statically or dynamically output size
  def id: ModifierId

  def encodedId: String = Base58.encode(id)

}

/**
  * It is supposed that all the modifiers (offchain transactions, blocks, blockheaders etc)
  * have identifiers of the some length fixed with the ModifierIdSize constant
  */
object NodeViewModifier {
  val DefaultIdSize = 32 // in bytes

  //TODO implement ModifierTypeId as a trait
  type ModifierTypeId = Byte
  type ModifierId = Array[Byte]

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}



trait PersistentNodeViewModifier[P <: Proposition, TX <: GenericTransaction[P]] extends NodeViewModifier {

  def parentId: ModifierId

  // with Dotty is would be Seq[TX] | Nothing
  def transactions: Option[Seq[TX]]
}