package co.topl.consensus.tine

import co.topl.models._

abstract class BlockStorage {

  def refresh(): Unit

  def add(block: BlockV2): Unit

  def store(key: TypedIdentifier, block: BlockV2): Unit

  def restoreBlock(id: SlotId): Option[BlockV2]

  def restoreHeader(id: SlotId): Option[BlockHeaderV2]

  def getBlock(id: SlotId): Option[BlockV2]

  def getHeader(id: SlotId): Option[BlockHeaderV2]

  def getIfPresent(id: SlotId): Option[BlockV2]

  def known(id: SlotId): Boolean

  def knownIfPresent(id: SlotId): Boolean

  def knownInCache(id: SlotId): Boolean

  def getIfInCache(id: SlotId): Option[BlockV2]

  def populateCache(id: SlotId): Unit

}
