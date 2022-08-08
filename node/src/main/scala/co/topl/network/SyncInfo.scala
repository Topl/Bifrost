package co.topl.network

import co.topl.nodeView.history.GenericHistory

/**
 * Syncing info provides information about starting points this node recommends another to start
 * synchronization from
 */
trait SyncInfo {

  /** Sequence of modifier ids and type ids */
  def startingPoints: GenericHistory.TypedModifierIds
}
