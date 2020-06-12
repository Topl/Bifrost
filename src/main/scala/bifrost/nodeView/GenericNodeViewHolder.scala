package bifrost.nodeView

import akka.actor.{Actor, ActorRef}
import bifrost.history.GenericHistory
import bifrost.history.GenericHistory.HistoryComparisonResult
import bifrost.mempool.MemoryPool
import bifrost.modifier.ModifierId
import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.BoxTransaction
import bifrost.modifier.transaction.bifrostTransaction.{CoinbaseTransaction, GenericTransaction, Transaction}
import bifrost.network.{ConnectedPeer, SyncInfo}
import bifrost.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import bifrost.nodeView.NodeViewModifier.ModifierTypeId
import bifrost.state.MinimalState
import bifrost.utils.Logging
import bifrost.utils.serialization.BifrostSerializer
import bifrost.wallet.Vault
import scorex.crypto.encode.Base58

import scala.collection.mutable
import scala.util.{Failure, Success}

trait GenericNodeViewHolder[T, P <: Proposition, TX <: BoxTransaction[P, T, BX], BX <: GenericBox[P, T], PMOD <: PersistentNodeViewModifier]
  extends Actor with Logging {

  import GenericNodeViewHolder.ReceivableMessages._
  import GenericNodeViewHolder._
  import bifrost.network.NodeViewSynchronizer.ReceivableMessages._

  type SI <: SyncInfo
  type HIS <: GenericHistory[P, TX, PMOD, SI, HIS]
  type MS <: MinimalState[T, P, BX, TX, PMOD, MS]
  type VL <: Vault[P, TX, PMOD, VL]
  type MP <: MemoryPool[TX, MP]

  type NodeView = (HIS, MS, VL, MP)

  val modifierCompanions: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]]

  val networkChunkSize: Int

  //todo: make configurable limited size
  private val modifiersCache = mutable.Map[ModifierId, (ConnectedPeer, PMOD)]()

  //mutable private node view instance
  private var nodeView: NodeView = restoreState().getOrElse(genesisState)

  private val subscribers = mutable.Map[GenericNodeViewHolder.EventType.Value, Seq[ActorRef]]()

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    processModifiers orElse
      transactionsProcessing orElse
      getCurrentInfo orElse
      getNodeViewChanges orElse
      nonsense

//    handleSubscribe orElse
//      compareViews orElse
//      readLocalObjects orElse
//      getCurrentInfo orElse
//      getSyncInfo orElse
//      compareSyncInfo orElse

  // ----------- MESSAGE PROCESSING FUNCTIONS
  protected def processModifiers: Receive = {
    case lm: LocallyGeneratedModifier[PMOD] =>
      log.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod)

    case ModifiersFromRemote(mods: Seq[PMOD]) =>
      mods.foreach(m => modifiersCache.put(m.id, m))

      log.debug(s"Cache size before: ${modifiersCache.size}")

      @tailrec
      def applyLoop(applied: Seq[PMOD]): Seq[PMOD] = {
        modifiersCache.popCandidate(history()) match {
          case Some(mod) =>
            pmodModify(mod)
            applyLoop(mod +: applied)
          case None =>
            applied
        }
      }

      val applied = applyLoop(Seq())
      val cleared = modifiersCache.cleanOverfull()

      context.system.eventStream.publish(ModifiersProcessingResult(applied, cleared))
      log.debug(s"Cache size after: ${modifiersCache.size}")
  }

  protected def transactionsProcessing: Receive = {

    case newTxs: NewTransactions[TX] =>
      newTxs.txs.foreach(txModify)

    case EliminateTransactions(ids) =>
      val updatedPool = memoryPool().filter(tx => !ids.contains(tx.id))
      updateNodeView(updatedMempool = Some(updatedPool))
      ids.foreach { id =>
        val e = new Exception("Became invalid")
        context.system.eventStream.publish(FailedTransaction(id, e, immediateFailure = false))
      }
  }

  protected def getCurrentInfo: Receive = {
    case GetDataFromCurrentView(f) =>
      sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
  }

  protected def getNodeViewChanges: Receive = {
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView._1.getReader)
      if (state) sender() ! ChangedState(nodeView._2.getReader)
      if (vault) sender() ! ChangedVault(nodeView._3.getReader)
      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
  }

  private def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"NodeViewHolder: got unexpected input $nonsense :: ${nonsense.getClass}")
  }

  /**
   *
  private def handleSubscribe: Receive = {
    case GenericNodeViewHolder.Subscribe(events) =>
      events.foreach { evt =>
        val current = subscribers.getOrElse(evt, Seq())
        subscribers.put(evt, current :+ sender())
      }
  }

  private def compareViews: Receive = {
    case CompareViews(sid, modifierTypeId, modifierIds) =>
      val ids = modifierTypeId match {
        case typeId: Byte if typeId == GenericTransaction.ModifierTypeId =>
          memoryPool().notIn(modifierIds)
        case _ =>
          modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(mid))
      }

      sender() ! RequestFromLocal(sid, modifierTypeId, ids)
  }

  private def readLocalObjects: Receive = {
    case GetLocalObjects(sid, modifierTypeId, modifierIds) =>
      val objs: Seq[NodeViewModifier] = modifierTypeId match {
        case typeId: Byte if typeId == GenericTransaction.ModifierTypeId =>
          memoryPool().getAll(modifierIds)
        case typeId: Byte =>
          modifierIds.flatMap(id => history().modifierById(id))
      }

      log.debug(s"Requested modifiers ${modifierIds.map(Base58.encode)}, sending: " + objs.map(_.id).map(Base58.encode))
      sender() ! ResponseFromLocal(sid, modifierTypeId, objs)
  }

  private def compareSyncInfo: Receive = {
    case OtherNodeSyncingInfo(remote, syncInfo: SI @unchecked) =>
      log.debug(s"Comparing remote info having starting points: ${syncInfo.startingPoints.map(_._2).map(Base58.encode).toList}")
      log.debug(s"Local side contains head: ${history().contains(syncInfo.startingPoints.map(_._2).head)}")

      val extensionOpt = history().continuationIds(syncInfo.startingPoints, networkChunkSize)
      val ext = extensionOpt.getOrElse(Seq())
      val comparison = history().compare(syncInfo)
      log.debug(s"Sending extension of length ${ext.length}: ${ext.map(_._2).map(Base58.encode).mkString(",")}")
      log.debug("Comparison with Remote. Remote is: " + comparison)

      val theyAreYounger = comparison == HistoryComparisonResult.Younger
      val notSendingBlocks = extensionOpt.isEmpty

      //if(notSendingBlocks && theyAreYounger) throw new Exception("Other node was younger but we didn't have blocks to send")

      if (notSendingBlocks && theyAreYounger) {
        log.debug(s"Error: Trying to sync local node with remote node. " +
          s"Failed to find common ancestor within block history. " +
          s"Check that you are attempting to sync to the correct version of the blockchain.")
      }

      sender() ! OtherNodeSyncingStatus(
        remote,
        comparison,
        syncInfo,
        history().syncInfo(true),
        extensionOpt
      )
  }

  private def getSyncInfo: Receive = {
    case GetSyncInfo =>
      sender() ! CurrentSyncInfo(history().syncInfo(false))
  }

   */

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /**
   * Hard-coded initial view all the honest nodes in a network are making progress from.
   */
  protected def genesisState: NodeView

  /**
   * Restore a local view during a node startup. If no any stored view found
   * (e.g. if it is a first launch of a node) None is to be returned
   */
  def restoreState(): Option[NodeView]

  private def history(): HIS = nodeView._1

  private def minimalState(): MS = nodeView._2

  private def vault(): VL = nodeView._3

  private def memoryPool(): MP = nodeView._4

  private def notifySubscribers[O <: NodeViewHolderEvent](eventType: EventType.Value, signal: O) =
    subscribers.getOrElse(eventType, Seq()).foreach(_ ! signal)

  private def txModify(tx: TX, source: Option[ConnectedPeer]) = {
    val updWallet = vault().scanOffchain(tx)
    memoryPool().put(tx) match {
      case Success(updPool) =>
        log.debug(s"Unconfirmed transaction $tx added to the mempool")
        nodeView = (history(), minimalState(), updWallet, updPool)
        notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx, source))

      case Failure(e) =>
        notifySubscribers(EventType.FailedTransaction, FailedTransaction[P, TX](tx, e, source))
    }
  }

  //noinspection ScalaStyle
  private def pmodModify(pmod: PMOD, source: Option[ConnectedPeer]): Unit = if (!history().contains(pmod.id)) {
    notifySubscribers(
      EventType.StartingPersistentModifierApplication,
      StartingPersistentModifierApplication[PMOD](pmod)
    )

    log.debug(s"Apply modifier to nodeViewHolder: ${pmod.id}")

    history().append(pmod) match {
      case Success((newHistory, progressInfo)) =>
        log.debug(s"Going to apply modifications: $progressInfo")

        // Modifier is in a best chain so apply
        if (progressInfo.toApply.nonEmpty) {

          val newStateTry = if (progressInfo.rollbackNeeded) {
            minimalState().rollbackTo(progressInfo.branchPoint.get).flatMap(_.applyModifiers(progressInfo.toApply))
          } else {
            minimalState().applyModifiers(progressInfo.toApply)
          }

          newStateTry match {
            case Success(newMinState) =>
              val rolledBackTxs = progressInfo.toRemove.flatMap(_.transactions).flatten

              val appliedMods = progressInfo.toApply

              val appliedTxs = appliedMods.flatMap(_.transactions).flatten
              var newMemPool = memoryPool()
              log.debug(s"${Console.GREEN}before newMemPool Size: ${newMemPool.size}${Console.RESET}")
              newMemPool = memoryPool().putWithoutCheck(rolledBackTxs).filter { tx => !tx.isInstanceOf[CoinbaseTransaction] &&
                !appliedTxs.exists(t => t.id sameElements tx.id) && newMinState.validate(tx).isSuccess
              }
              val validUnconfirmed = newMemPool.take(100)
              log.debug(s"${Console.GREEN}Re-Broadcast unconfirmed TXs: ${validUnconfirmed.map(tx => Base58.encode(tx.id)).toList}${Console.RESET}")
              validUnconfirmed.foreach(tx => { if(tx.isInstanceOf[CoinbaseTransaction]) {log.debug(s"${Console.RED}Attempting to rebroadcast Coinbase transaction" + tx)}
                notifySubscribers(EventType.SuccessfulTransaction, SuccessfulTransaction[P, TX](tx, None))})
              log.debug(s"${Console.GREEN}newMemPool Size: ${newMemPool.size}${Console.RESET}")

              //YT NOTE - deprecate in favor of optional nodeKeys for TokenBoxRegistry - wallet boxes still being used by Forger
              //we consider that vault always able to perform a rollback needed
              val newVault = if (progressInfo.rollbackNeeded) {
                vault().rollback(progressInfo.branchPoint.get).get.scanPersistent(appliedMods)
              } else {
                vault().scanPersistent(appliedMods)
              }

              log.debug(s"Persistent modifier ${pmod.id} applied successfully")
              nodeView = (newHistory, newMinState, newVault, newMemPool)
              notifySubscribers(EventType.SuccessfulPersistentModifier, SuccessfulModification[P, TX, PMOD](pmod, source))

            case Failure(e) =>
              val newHistoryCancelled = newHistory.drop(progressInfo.appendedId)
              nodeView = (newHistoryCancelled, minimalState(), vault(), memoryPool())

              log.warn(s"Can`t apply persistent modifier (id: ${pmod.id}, contents: $pmod) to minimal state", e)
              notifySubscribers(EventType.FailedPersistentModifier, FailedModification[P, TX, PMOD](pmod, e, source))
          }
        }
      case Failure(e) =>
        e.printStackTrace()
    }
  } else {
    log.warn(s"Trying to apply modifier ${pmod.id} that's already in history")
  }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object GenericNodeViewHolder {

//  case object GetSyncInfo
//
//  case class CurrentSyncInfo[SI <: SyncInfo](syncInfo: SyncInfo)
//
//  case object GetCurrentView
//
//  object EventType extends Enumeration {
//    //finished modifier application, successful of failed
//    val FailedTransaction = Value(1)
//    val FailedPersistentModifier = Value(2)
//    val SuccessfulTransaction = Value(3)
//    val SuccessfulPersistentModifier = Value(4)
//
//    //starting persistent modifier application. The application could be slow
//    val StartingPersistentModifierApplication = Value(5)
//  }
//
//  //a command to subscribe for events
//  case class Subscribe(events: Seq[EventType.Value])
//
//  //trait NodeViewHolderEvent
//
////  case class OtherNodeSyncingStatus[SI <: SyncInfo](peer: ConnectedPeer,
////                                                    status: GenericHistory.HistoryComparisonResult.Value,
////                                                    remoteSyncInfo: SI,
////                                                    localSyncInfo: SI,
////                                                    extension: Option[Seq[(ModifierTypeId, ModifierId)]])
//
//  //node view holder starting persistent modifier application
//  //case class StartingPersistentModifierApplication[P <: Proposition, TX <: GenericTransaction[P], PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent
//
//  //hierarchy of events regarding modifiers application outcome
//  trait ModificationOutcome extends NodeViewHolderEvent {
//    val source: Option[ConnectedPeer]
//  }
//
//  case class FailedTransaction[P <: Proposition, TX <: GenericTransaction[P]]
//  (transaction: TX, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome
//
//  case class FailedModification[P <: Proposition, TX <: GenericTransaction[P], PMOD <: PersistentNodeViewModifier]
//  (modifier: PMOD, error: Throwable, override val source: Option[ConnectedPeer]) extends ModificationOutcome
//
//  case class SuccessfulTransaction[P <: Proposition, TX <: GenericTransaction[P]]
//  (transaction: TX, override val source: Option[ConnectedPeer]) extends ModificationOutcome
//
//  case class SuccessfulModification[P <: Proposition, TX <: GenericTransaction[P], PMOD <: PersistentNodeViewModifier]
//  (modifier: PMOD, override val source: Option[ConnectedPeer]) extends ModificationOutcome





  object ReceivableMessages {

      // Explicit request of NodeViewChange events of certain types.
      case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

      case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

      // Modifiers received from the remote peer with new elements in it
      case class ModifiersFromRemote[PM <: PersistentNodeViewModifier](modifiers: Iterable[PM])

      sealed trait NewTransactions[TX <: Transaction]{
        val txs: Iterable[TX]
      }

      case class LocallyGeneratedTransaction[TX <: Transaction](tx: TX) extends NewTransactions[TX] {
        override val txs: Iterable[TX] = Iterable(tx)
      }

      case class TransactionsFromRemote[TX <: Transaction](txs: Iterable[TX]) extends NewTransactions[TX]

      case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)

      case class EliminateTransactions(ids: Seq[ModifierId])

    }

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

  case class DownloadRequest(modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId) extends NodeViewHolderEvent
}