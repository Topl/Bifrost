package bifrost.network

import bifrost.consensus.History
import bifrost.serialization.BytesSerializable

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo extends BytesSerializable {
  def answer: Boolean
  def startingPoints: History.ModifierIds
}
