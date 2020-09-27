package bifrost.nodeView

import akka.actor.Actor
import bifrost.history.GenericHistory
import bifrost.history.GenericHistory.ProgressInfo
import bifrost.mempool.MemoryPool
import bifrost.modifier.ModifierId
import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.network.message.SyncInfo
import bifrost.settings.AppSettings
import bifrost.state.{ MinimalState, StateReader, TransactionValidation }
import bifrost.utils.{ BifrostEncoding, Logging }
import bifrost.wallet.Vault

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
  * Composite local view of the node
  *
  * Contains instances for History, MinimalState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view(the instances are to be performed atomically.
  *
  * @tparam TX
  * @tparam PMOD
  */
trait GenericNodeViewHolder [ BX   <: GenericBox[_ <: Proposition, _],
                              TX   <: Transaction,
                              PMOD <: PersistentNodeViewModifier,
                              HIS  <: GenericHistory[PMOD, _ <: SyncInfo, HIS],
                              MS   <: MinimalState[BX, PMOD, MS],
                              VL   <: Vault[TX, PMOD, VL],
                              MP   <: MemoryPool[TX, MP]
                            ] extends Actor with Logging with BifrostEncoding {

  // Import the types of messages this actor can RECEIVE
  import GenericNodeViewHolder.ReceivableMessages._

  // Import the types of messages this actor can SEND
  import bifrost.network.NodeViewSynchronizer.ReceivableMessages._

  type NodeView = (HIS, MS, VL, MP)

  case class UpdateInformation(history: HIS,
                               state: MS,
                               failedMod: Option[PMOD],
                               alternativeProgressInfo: Option[ProgressInfo[PMOD]],
                               suffix: IndexedSeq[PMOD])

  val settings: AppSettings

  /**
    * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
    */
  protected lazy val modifiersCache: ModifiersCache[PMOD, HIS] =
    new DefaultModifiersCache[PMOD, HIS](settings.network.maxModifiersCacheSize)

  /**
    * The main data structure a node software is taking care about, a node view consists
    * of four elements to be updated atomically: history (log of persistent modifiers),
    * state (result of log's modifiers application to pre-historical(genesis) state,
    * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
    */
  protected var nodeView: NodeView = restoreState().getOrElse(genesisState)

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    processModifiers orElse
    transactionsProcessing orElse
    getCurrentInfo orElse
    getNodeViewChanges orElse
    nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS

  protected def processModifiers: Receive = {
    case ModifiersFromRemote(mods: Seq[PMOD]) =>
      processRemoteModifiers(mods)

    case lm: LocallyGeneratedModifier[PMOD] =>
      log.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod)
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

  protected def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"NodeViewHolder: got unexpected input $nonsense from ${sender()}")
  }


  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  def restoreState(): Option[NodeView]

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  protected def genesisState: NodeView

  protected def history(): HIS = nodeView._1

  protected def minimalState(): MS = nodeView._2

  protected def vault(): VL = nodeView._3

  protected def memoryPool(): MP = nodeView._4

  /**
   * Handles adding remote modifiers to the default cache and then attempts to apply them to the history.
   * Since this cache is unordered, we continue to loop through the cache until it's size remains constant.
   * This indicates that no more modifiers in the cache can be appended into history
   *
   * @param mods recieved persistent modifiers from the remote peer
   */
  protected def processRemoteModifiers(mods: Seq[PMOD]): Unit = {
    /** First-order loop that tries to pop blocks out of the cache and apply them into history */
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

    /** Second-order loop that continues to call the first-order loop until the size of the cache remains constant */
    @tailrec
    def applyAllApplicable(acc: Seq[PMOD], startSize: Int): Seq[PMOD] = {
      val applied = applyLoop(acc)
      if (modifiersCache.size == startSize) applied
      else applyAllApplicable(applied, modifiersCache.size)
    }

    // add all newly received modifiers to the cache
    mods.foreach(m => modifiersCache.put(m.id, m))

    // record the initial size for comparison
    val initialSize = modifiersCache.size

    // begin looping through the cache and trying to apply all applicable modifiers
    val applied = applyAllApplicable(Seq(), initialSize)

    // clean the cache if it is growing too large
    val cleared = modifiersCache.cleanOverfull()

    context.system.eventStream.publish(ModifiersProcessingResult(applied, cleared))
    log.debug(s"Cache size before: $initialSize")
    log.debug(s"Cache size after: ${modifiersCache.size}")
  }

  protected def txModify(tx: TX): Unit = {
    //todo: async validation?
    val errorOpt: Option[Throwable] = minimalState() match {
      case txValidator: TransactionValidation[TX] =>
        txValidator.validate(tx) match {
          case Success(_) => None
          case Failure(e) => Some(e)
        }
      case _ => None
    }

    errorOpt match {
      case None =>
        memoryPool().put(tx) match {
          case Success(newPool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            val newVault = vault().scanOffchain(tx)
            updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
            context.system.eventStream.publish(SuccessfulTransaction[TX](tx))

          case Failure(e) =>
            context.system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
        }

      case Some(e) =>
        context.system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
    }
  }

  //todo: update state in async way?
  protected def pmodModify(pmod: PMOD): Unit =
    if (!history().contains(pmod.id)) {
      context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))

      log.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")

      history().append(pmod) match {
        case Success((historyBeforeStUpdate, progressInfo)) =>
          log.debug(s"Going to apply modifications to the state: $progressInfo")
          context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
          context.system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))

          if (progressInfo.toApply.nonEmpty) {
            val (newHistory, newStateTry, blocksApplied) =
              updateState(historyBeforeStUpdate, minimalState(), progressInfo, IndexedSeq())

            newStateTry match {
              case Success(newMinState) =>
                val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, memoryPool(), newMinState)

                //we consider that vault always able to perform a rollback needed
                @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
                val newVault = if (progressInfo.chainSwitchingNeeded) {
                  vault().rollback(progressInfo.branchPoint.get).get
                } else vault()
                blocksApplied.foreach(newVault.scanPersistent)

                log.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
                updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))


              case Failure(e) =>
                log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to minimal state", e)
                updateNodeView(updatedHistory = Some(newHistory))
                context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
            }
          } else {
            requestDownloads(progressInfo)
            updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
          }
        case Failure(e) =>
          log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history", e)
          context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
      }
    } else {
      log.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")
    }

  protected def extractTransactions(mod: PMOD): Seq[TX] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[TX] => tcm.transactions
    case _ => Seq()
  }

  private def requestDownloads(pi: ProgressInfo[PMOD]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      context.system.eventStream.publish(DownloadRequest(tid, id))
    }

  private def trimChainSuffix(suffix: IndexedSeq[PMOD], rollbackPoint: ModifierId): IndexedSeq[PMOD] = {
    val idx = suffix.indexWhere(_.id == rollbackPoint)
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }


  /** Below is a description of how state updates are managed */
  /** ------------------------------------------------------------------------------------------------------------------- /
  Assume that history knows the following blocktree:

           G
          / \
         *   G
        /     \
       *       G

    where path with G-s is about canonical chain (G means semantically valid modifier), path with * is sidechain (* means
    that semantic validity is unknown). New modifier is coming to the sidechain, it sends rollback to the root +
    application of the sidechain to the state. Assume that state is finding that some modifier in the sidechain is
    incorrect:

           G
          / \
         G   G
        /     \
       B       G
      /
     *

  In this case history should be informed about the bad modifier and it should retarget state

    //todo: improve the comment below

    We assume that we apply modifiers sequentially (on a single modifier coming from the network or generated locally),
    and in case of failed application of some modifier in a progressInfo, rollback point in an alternative should be not
    earlier than a rollback point of an initial progressInfo.
  / ---------------------------------------------------------------------------------------------------------------------- **/

  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[PMOD],
                          suffixApplied: IndexedSeq[PMOD]): (HIS, Try[MS], Seq[PMOD]) = {
    requestDownloads(progressInfo)

    val (stateToApplyTry: Try[MS], suffixTrimmed: IndexedSeq[PMOD]) = if (progressInfo.chainSwitchingNeeded) {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val branchingPoint = progressInfo.branchPoint.get //todo: .get
      if (state.version != branchingPoint) {
        state.rollbackTo(branchingPoint) -> trimChainSuffix(suffixApplied, branchingPoint)
      } else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        val stateUpdateInfo = applyState(history, stateToApply, suffixTrimmed, progressInfo)

        stateUpdateInfo.failedMod match {
          case Some(_) =>
            @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
            val alternativeProgressInfo = stateUpdateInfo.alternativeProgressInfo.get
            updateState(stateUpdateInfo.history, stateUpdateInfo.state, alternativeProgressInfo, stateUpdateInfo.suffix)
          case None => (stateUpdateInfo.history, Success(stateUpdateInfo.state), stateUpdateInfo.suffix)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  /**
   * Update NodeView with new components and notify subscribers of changed components
   *
   * @param updatedHistory
   * @param updatedState
   * @param updatedVault
   * @param updatedMempool
   */
  protected def updateNodeView(updatedHistory: Option[HIS] = None,
                               updatedState: Option[MS] = None,
                               updatedVault: Option[VL] = None,
                               updatedMempool: Option[MP] = None): Unit = {
    val newNodeView = (updatedHistory.getOrElse(history()),
      updatedState.getOrElse(minimalState()),
      updatedVault.getOrElse(vault()),
      updatedMempool.getOrElse(memoryPool()))
    if (updatedHistory.nonEmpty) {
      context.system.eventStream.publish(ChangedHistory(newNodeView._1.getReader))
    }
    if (updatedState.nonEmpty) {
      context.system.eventStream.publish(ChangedState(newNodeView._2.getReader))
    }
    if (updatedVault.nonEmpty) {
      context.system.eventStream.publish(ChangedVault(newNodeView._3.getReader))
    }
    if (updatedMempool.nonEmpty) {
      context.system.eventStream.publish(ChangedMempool(newNodeView._4.getReader))
    }
    nodeView = newNodeView
  }

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  protected def updateMemPool(blocksRemoved: Seq[PMOD], blocksApplied: Seq[PMOD], memPool: MP, state: MS): MP = {
    val rolledBackTxs = blocksRemoved.flatMap(extractTransactions)

    val appliedTxs = blocksApplied.flatMap(extractTransactions)

    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id == tx.id) && {
        state match {
          case v: TransactionValidation[TX] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  /**
    * Attempts to update the local view of state by applying a set of blocks
    *
    * @param history the initial view of history prior to updating
    * @param stateToApply the initial view of state prior to updating
    * @param suffixTrimmed ???
    * @param progressInfo class with blocks that need to be applied to state
    * @return
    */
  protected def applyState(history: HIS,
                           stateToApply: MS,
                           suffixTrimmed: IndexedSeq[PMOD],
                           progressInfo: ProgressInfo[PMOD]): UpdateInformation = {

    val updateInfoInit = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)

    progressInfo.toApply.foldLeft(updateInfoInit) { case (updateInfo, modToApply) =>
      if (updateInfo.failedMod.isEmpty) {
        updateInfo.state.applyModifier(modToApply) match {
          case Success(stateAfterApply) =>
            val newHis = history.reportModifierIsValid(modToApply)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
            UpdateInformation(newHis, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)

          case Failure(e) =>
            val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
            UpdateInformation(newHis, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      } else updateInfo
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object GenericNodeViewHolder {

  object ReceivableMessages {

    // Explicit request of NodeViewChange events of certain types.
    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

    // Modifiers received from the remote peer with new elements in it
    case class ModifiersFromRemote[PM <: PersistentNodeViewModifier](modifiers: Iterable[PM])

    sealed trait NewTransactions[TX <: Transaction]{val txs: Iterable[TX]}

    case class LocallyGeneratedTransaction[TX <: Transaction](tx: TX) extends NewTransactions[TX] {
      override val txs: Iterable[TX] = Iterable(tx)
    }

    case class TransactionsFromRemote[TX <: Transaction](txs: Iterable[TX]) extends NewTransactions[TX]

    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)

    case class EliminateTransactions(ids: Seq[ModifierId])

  }

}
