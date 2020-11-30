package co.topl.network.message

import co.topl.nodeView.history.GenericHistory

/**
  * Syncing info provides information about starting points this node recommends another to start
  * synchronization from
  */
trait SyncInfo {
  def startingPoints: GenericHistory.ModifierIds
}
