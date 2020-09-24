package bifrost.network.message

import bifrost.history.GenericHistory

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo {
  def startingPoints: GenericHistory.ModifierIds
}
