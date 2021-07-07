package co.topl.nodeView

import akka.Done
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{Actor, ActorRef, ActorSystem, Props, Stash}
import akka.dispatch.Dispatchers
import akka.pattern._
import akka.util.Timeout
import cats.data.EitherT
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.catsakka.AskException
import co.topl.consensus.Forger
import co.topl.consensus.Forger.ReceivableMessages.GenerateGenesis
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.serialization.BlockSerializer
import co.topl.modifier.block.{Block, PersistentNodeViewModifier, TransactionCarryingPersistentNodeViewModifier}
import co.topl.modifier.box.ProgramId
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.serialization.TransactionSerializer
import co.topl.modifier.transaction.validation.implicits._
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.network.NodeViewSynchronizer.ReceivableMessages._
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.history.GenericHistory.ProgressInfo
import co.topl.nodeView.history.{GenericHistory, History, HistoryReader}
import co.topl.nodeView.mempool.{MemPool, MemPoolReader}
import co.topl.nodeView.state.{State, StateReader}
import co.topl.settings.{AppContext, AppSettings, NodeViewReady}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.encode.Base58
import co.topl.utils.serialization.BifrostSerializer
import co.topl.utils.{AsyncRunner, Logging}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Composite local view of the node
 *
 * Contains instances for History, MinimalState, Vault, MemoryPool.
 * The instances are read-only for external world.
 * Updates of the composite view(the instances are to be performed atomically.
 */
class NodeViewHolder(settings: AppSettings, appContext: AppContext)(implicit np: NetworkPrefix)
    extends Actor
    with Logging
    with Stash {

  import NodeViewHolder.ReceivableMessages._
  import context.{dispatcher, system}

  private val blockingDispatcher = context.system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)

  /**
   * The main data structure a node software is taking care about, a node view consists
   * of four elements to be updated atomically: history (log of persistent modifiers),
   * state (result of log's modifiers application to pre-historical(genesis) state,
   * user-specific information stored in vault (it could be e.g. a wallet), and a memory pool.
   */
  private[nodeView] var nodeView: NodeView = _

  private[nodeView] var nodeViewWriter: NodeViewWriter = _

  private var writerRunner: AsyncRunner = _

  /** Define actor control behavior */
  override def preStart(): Unit = {
    nodeView = restoreState().getOrElse(genesisState)

    nodeViewWriter = new NodeViewWriter(settings, appContext, nodeView)

    writerRunner = AsyncRunner("node-view-writer")(system = context.system.toTyped, timeout = Timeout(10.minutes))

    // subscribe to particular messages this actor expects to receive
    context.system.eventStream.subscribe(self, classOf[LocallyGeneratedModifier[Block]])

    log.info(s"${Console.YELLOW}NodeViewHolder publishing ready signal${Console.RESET}")
    context.system.eventStream.publish(NodeViewReady(this.self))
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100) // this actor shouldn't be restarted at all so kill the whole app if that happened
  }

  override def postStop(): Unit = {
    log.info(s"${Console.RED}Application is going down NOW!${Console.RESET}")

    nodeView.history.closeStorage() // close History storage
    nodeView.state.closeStorage() // close State storage
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    getNodeViewChanges orElse runWithNodeView orElse nodeViewChanged orElse modification orElse nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS

  private def runWithNodeView: Receive = { case RunWithNodeView(f) =>
    Future(f(ReadableNodeView(nodeView.history.getReader, nodeView.state.getReader, nodeView.mempool.getReader)))(
      blockingDispatcher
    )
      .pipeTo(sender())
  }

  protected def getNodeViewChanges: Receive = { case GetNodeViewChanges(history, state, mempool) =>
    if (history) sender() ! ChangedHistory(nodeView.history.getReader)
    if (state) sender() ! ChangedState(nodeView.state.getReader)
    if (mempool) sender() ! ChangedMempool(nodeView.mempool.getReader)
  }

  private def nodeViewChanged: Receive = { case NodeViewChanged(updated) =>
    nodeView = updated
  }

  private def modification: Receive = {
    case m: ModifiersFromRemote[_] =>
      writerRunner
        .run(() =>
          nodeViewWriter.handle(
            NodeViewWriter.Messages.WriteBlocks(m.modifiers.asInstanceOf[Iterable[Block]])
          )
        )
        .map(NodeViewChanged)
        .pipeTo(self)
    case m: LocallyGeneratedModifier[_] =>
      writerRunner
        .run(() =>
          nodeViewWriter.handle(
            NodeViewWriter.Messages.WriteBlock(m.pmod.asInstanceOf[Block])
          )
        )
        .map(NodeViewChanged)
        .pipeTo(self)
    case m: NewTransactions =>
      writerRunner
        .run(() =>
          nodeViewWriter.handle(
            NodeViewWriter.Messages.WriteTransactions(m.txs)
          )
        )
        .map(NodeViewChanged)
        .pipeTo(self)
    case m: EliminateTransactions =>
      writerRunner
        .run(() =>
          nodeViewWriter.handle(
            NodeViewWriter.Messages.EliminateTransactions(m.ids)
          )
        )
        .map(NodeViewChanged)
        .pipeTo(self)
  }

  protected def nonsense: Receive = { case nonsense: Any =>
    log.warn(s"NodeViewHolder: got unexpected input $nonsense from ${sender()}")
  }

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /**
   * Restore a local view during a node startup. If no any stored view found
   * (e.g. if it is a first launch of a node) None is to be returned
   */
  private def restoreState(): Option[NodeView] =
    if (State.exists(settings)) {
      Some(
        NodeView(
          History.readOrGenerate(settings),
          State.readOrGenerate(settings),
          MemPool.emptyPool
        )
      )
    } else None

  /** Hard-coded initial view all the honest nodes in a network are making progress from. */
  private[nodeView] def genesisState(implicit timeout: Timeout = 10 seconds): NodeView = {
    // this has to be resolved before moving on with the rest of the initialization procedure
    val genesisBlock = Await.result(getGenesisBlock, timeout.duration)

    // generate the nodeView and return
    NodeView(
      History.readOrGenerate(settings).append(genesisBlock).get._1,
      State.genesisState(settings, Seq(genesisBlock)),
      MemPool.emptyPool
    )
  }

  private def getGenesisBlock(implicit timeout: Timeout): Future[Block] =
    // need to lookup the actor reference for the forger
    context.actorSelection("../" + Forger.actorName).resolveOne().flatMap { consensusRef =>
      // if a reference was found, ask for the genesis block
      (consensusRef ? GenerateGenesis).mapTo[Try[Block]].map {
        case Success(block) => block
        case Failure(ex) =>
          throw new Error(s"${Console.RED}Failed to initialize genesis due to error${Console.RESET} $ex")
      }
    }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object NodeViewHolder {
  val actorName = "nodeViewHolder"

  case class UpdateInformation[HIS, MS, PMOD <: PersistentNodeViewModifier](
    history:                 HIS,
    state:                   MS,
    failedMod:               Option[PMOD],
    alternativeProgressInfo: Option[ProgressInfo[PMOD]],
    suffix:                  IndexedSeq[PMOD]
  )

  object ReceivableMessages {

    // Explicit request of NodeViewChange events of certain types.
    case class GetNodeViewChanges(history: Boolean, state: Boolean, mempool: Boolean)

    case class RunWithNodeView[T](f: ReadableNodeView => T)

    // Retrieve data from current view
    case object GetDataFromCurrentView

    // Modifiers received from the remote peer with new elements in it
    case class ModifiersFromRemote[PMOD <: PersistentNodeViewModifier](modifiers: Iterable[PMOD])

    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)

    private[nodeView] case object DoneProcessingModifiers

    private[nodeView] case class NodeViewChanged(nodeView: NodeView)

    sealed trait NewTransactions { val txs: Iterable[Transaction.TX] }

    case class LocallyGeneratedTransaction(tx: Transaction.TX) extends NewTransactions {
      override val txs: Iterable[Transaction.TX] = Iterable(tx)
    }

    case class TransactionsFromRemote(txs: Iterable[Transaction.TX]) extends NewTransactions

    case class EliminateTransactions(ids: Seq[ModifierId])

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object NodeViewHolderRef {

  def props(settings: AppSettings, appContext: AppContext): Props =
    Props(new NodeViewHolder(settings, appContext)(appContext.networkType.netPrefix))
      .withDispatcher(Dispatchers.DefaultBlockingDispatcherId)

  def apply(name: String, settings: AppSettings, appContext: AppContext)(implicit
    system:       ActorSystem
  ): ActorRef =
    system.actorOf(props(settings, appContext), name)
}

case class NodeView(history: History, state: State, mempool: MemPool)

case class ReadableNodeView(
  history: HistoryReader[Block, BifrostSyncInfo],
  state:   StateReader[ProgramId, Address],
  memPool: MemPoolReader[Transaction.TX]
)

private class NodeViewWriter(settings: AppSettings, appContext: AppContext, initialNodeView: NodeView)(implicit
  np:                                  NetworkPrefix,
  system:                              ActorSystem
) {

  import NodeViewHolder._
  import NodeViewWriter.Messages

  private[nodeView] var nodeView: NodeView = initialNodeView

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Cache for modifiers. If modifiers are coming out-of-order, they are to be stored in this cache.
   */
  protected lazy val modifiersCache: ModifiersCache[Block, History] =
    new DefaultModifiersCache[Block, History](settings.network.maxModifiersCacheSize)

  lazy val modifierCompanions: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]] =
    Map(Block.modifierTypeId -> BlockSerializer, Transaction.modifierTypeId -> TransactionSerializer)

  def handle(message: NodeViewWriter.Message): NodeView = {
    message match {
      case Messages.WriteBlock(block) =>
        log.info(s"Got locally generated modifier ${block.id} of type ${block.modifierTypeId}")
        nodeView = pmodModify(block, nodeView)
      case Messages.WriteBlocks(blocks) =>
        nodeView = processRemoteModifiers(blocks, nodeView)
      case Messages.WriteTransactions(transactions) =>
        nodeView = transactions.foldLeft(nodeView)((view, tx) => txModify(tx, view))
      case Messages.EliminateTransactions(ids) =>
        nodeView = eliminateTransactions(ids, nodeView)
    }

    nodeView
  }

  /**
   * Handles adding remote modifiers to the default cache and then attempts to apply them to the history.
   * Since this cache is unordered, we continue to loop through the cache until it's size remains constant.
   * This indicates that no more modifiers in the cache can be appended into history
   *
   * @param mods recieved persistent modifiers from the remote peer
   */
  private def processRemoteModifiers(mods: Iterable[Block], nodeView: NodeView): NodeView = {

    /** First-order loop that tries to pop blocks out of the cache and apply them into history */
    @tailrec
    def applyLoop(applied: Seq[Block], nodeView: NodeView): (Seq[Block], NodeView) =
      modifiersCache.popCandidate(nodeView.history) match {
        case Some(mod) =>
          applyLoop(mod +: applied, pmodModify(mod, nodeView))
        case None =>
          (applied, nodeView)
      }

    /** Second-order loop that continues to call the first-order loop until the size of the cache remains constant */
    @tailrec
    def applyAllApplicable(acc: Seq[Block], startSize: Int, nodeView: NodeView): (Seq[Block], NodeView) = {
      val (applied, newNodeView) = applyLoop(acc, nodeView)
      if (modifiersCache.size == startSize) (applied, newNodeView)
      else applyAllApplicable(applied, modifiersCache.size, newNodeView)
    }

    // add all newly received modifiers to the cache
    mods.toSeq.sortBy(_.height).foreach(m => modifiersCache.put(m.id, m))

    // record the initial size for comparison
    val initialSize = modifiersCache.size

    // begin looping through the cache and trying to apply all applicable modifiers
    val (applied, newNodeView) = applyAllApplicable(Seq(), initialSize, nodeView)

    // clean the cache if it is growing too large
    val cleared = modifiersCache.cleanOverfull()

    system.eventStream.publish(ModifiersProcessingResult(applied, cleared))
    log.debug(s"Cache size before: $initialSize")
    log.debug(s"Cache size after: ${modifiersCache.size}")

    newNodeView
  }

  private def txModify(tx: Transaction.TX, nodeView: NodeView): NodeView =
    tx.syntacticValidation.toEither match {
      case Right(_) =>
        nodeView.mempool.put(tx, appContext.timeProvider.time) match {
          case Success(pool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            system.eventStream.publish(SuccessfulTransaction[Transaction.TX](tx))
            nodeView.copy(mempool = pool)

          case Failure(e) =>
            system.eventStream.publish(FailedTransaction(tx.id, e, immediateFailure = true))
            nodeView
        }

      case Left(e) =>
        system.eventStream.publish(
          FailedTransaction(tx.id, new Exception(e.head.toString), immediateFailure = true)
        )
        nodeView
    }

  private def pmodModify(pmod: Block, nodeView: NodeView): NodeView =
    if (!nodeView.history.contains(pmod.id)) {
      system.eventStream.publish(StartingPersistentModifierApplication(pmod))

      // check that the transactions are semantically valid
      if (pmod.transactions.forall(_.semanticValidation(nodeView.state).isValid)) {
        log.info(s"Apply modifier ${pmod.id} of type ${pmod.modifierTypeId} to nodeViewHolder")

        // append the block to history
        nodeView.history.append(pmod) match {
          case Success((historyBeforeStUpdate, progressInfo)) =>
            log.debug(s"Going to apply modifications to the state: $progressInfo")
            system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
            system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))

            if (progressInfo.toApply.nonEmpty) {
              val (newHistory, newStateTry, blocksApplied) =
                updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())

              newStateTry match {
                case Success(newMinState) =>
                  val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, nodeView.mempool, newMinState)
                  log.info(s"Persistent modifier ${pmod.id} applied successfully")
                  notifyUpdate(newHistory)
                  notifyUpdate(newMinState)
                  notifyUpdate(newMemPool)
                  NodeView(newHistory, newMinState, newMemPool)

                case Failure(e) =>
                  log.error(s"Can`t apply persistent modifier (id: ${pmod.id}, contents: $pmod) to minimal state", e)
                  system.eventStream.publish(SemanticallyFailedModification(pmod, e))
                  notifyUpdate(newHistory)
                  nodeView.copy(history = newHistory)
              }
            } else {
              requestDownloads(progressInfo)
              notifyUpdate(historyBeforeStUpdate)
              nodeView.copy(history = historyBeforeStUpdate)
            }
          case Failure(e) =>
            log.error(s"Can`t apply persistent modifier (id: ${pmod.id}, contents: $pmod) to history", e)
            system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
            nodeView
        }
      } else {
        log.warn(s"Trying to apply modifier ${pmod.id} containing invalid transactions")
        nodeView
      }
    } else {
      log.warn(s"Trying to apply modifier ${pmod.id} that's already in history")
      nodeView
    }

  /**
   * @param mod - the block to retrieve transactions from
   * @return the sequence of transactions from a block
   */
  private def extractTransactions(mod: Block): Seq[Transaction.TX] = mod match {
    case tcm: TransactionCarryingPersistentNodeViewModifier[_] => tcm.transactions
    case _                                                     => Seq()
  }

  private def requestDownloads(pi: ProgressInfo[Block]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      system.eventStream.publish(DownloadRequest(tid, id))
    }

  private def trimChainSuffix(suffix: IndexedSeq[Block], rollbackPoint: ModifierId): IndexedSeq[Block] = {
    val idx = suffix.indexWhere(_.id == rollbackPoint)
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  /** Below is a description of how state updates are managed */
  /**
   * --------------------------------------------------------------------------------------------------------------- /
   *  Assume that history knows the following blocktree:
   *
   *           G
   *          / \
   *   G
   *        /     \
   *       G
   *
   *    where path with G-s is about canonical chain (G means semantically valid modifier), path with * is sidechain
   *    (* means that semantic validity is unknown). New modifier is coming to the sidechain, it sends rollback to
   *    the root + application of the sidechain to the state. Assume that state is finding that some modifier in the
   *    sidechain is incorrect:
   *
   *           G
   *          / \
   *         G   G
   *        /     \
   *       B       G
   *      /
   *
   *  In this case history should be informed about the bad modifier and it should retarget state
   *
   *    //todo: improve the comment below
   *
   *    We assume that we apply modifiers sequentially (on a single modifier coming from the network or generated locally),
   *    and in case of failed application of some modifier in a progressInfo, rollback point in an alternative should be not
   *    earlier than a rollback point of an initial progressInfo.
   *  / --------------------------------------------------------------------------------------------------------------- *
   */

  @tailrec
  private def updateState(
    history:       History,
    state:         State,
    progressInfo:  ProgressInfo[Block],
    suffixApplied: IndexedSeq[Block]
  ): (History, Try[State], Seq[Block]) = {

    requestDownloads(progressInfo)

    val (stateToApplyTry: Try[State], suffixTrimmed: IndexedSeq[Block]) = if (progressInfo.chainSwitchingNeeded) {
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
        system.eventStream.publish(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  private def notifyUpdate(updatedHistory: History): Unit =
    system.eventStream.publish(ChangedHistory(updatedHistory.getReader))

  private def notifyUpdate(updatedState: State): Unit =
    system.eventStream.publish(ChangedState(updatedState.getReader))

  private def notifyUpdate(updatedMempool: MemPool): Unit =
    system.eventStream.publish(ChangedMempool(updatedMempool.getReader))

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  private def updateMemPool(
    blocksRemoved: Seq[Block],
    blocksApplied: Seq[Block],
    memPool:       MemPool,
    state:         State
  ): MemPool = {
    // drop the first two transactions, since these are the reward transactions and invalid
    val rolledBackTxs = blocksRemoved.flatMap(b => extractTransactions(b).drop(2))

    val appliedTxs = blocksApplied.flatMap(extractTransactions)

    memPool
      .putWithoutCheck(rolledBackTxs, appContext.timeProvider.time)
      .filter { tx =>
        !appliedTxs.exists(t => t.id == tx.id) && {
          tx.syntacticValidation.isValid
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
  private def applyState(
    history:       History,
    stateToApply:  State,
    suffixTrimmed: IndexedSeq[Block],
    progressInfo:  ProgressInfo[Block]
  ): UpdateInformation[History, State, Block] = {

    val updateInfoInit = UpdateInformation[History, State, Block](history, stateToApply, None, None, suffixTrimmed)

    progressInfo.toApply.foldLeft(updateInfoInit) { case (updateInfo, modToApply) =>
      if (updateInfo.failedMod.isEmpty) {
        updateInfo.state.applyModifier(modToApply) match {
          case Success(stateAfterApply) =>
            val newHis = history.reportModifierIsValid(modToApply)
            system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
            UpdateInformation(newHis, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)

          case Failure(e) =>
            val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
            UpdateInformation(newHis, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      } else updateInfo
    }
  }

  private def eliminateTransactions(ids: Seq[ModifierId], nodeView: NodeView): NodeView = {
    log.debug(s"${Console.YELLOW} Removing transactions with ids: $ids from mempool${Console.RESET}")
    val updatedPool = nodeView.mempool.filter(tx => !ids.contains(tx.id))
    notifyUpdate(updatedPool)
    ids.foreach { id =>
      val e = new Exception("Became invalid")
      system.eventStream.publish(FailedTransaction(id, e, immediateFailure = false))
    }
    nodeView.copy(mempool = updatedPool)
  }
}

private object NodeViewWriter {

  sealed abstract class Message

  object Messages {
    case class WriteBlocks(blocks: Iterable[Block]) extends Message
    case class WriteBlock(block: Block) extends Message
    case class WriteTransactions(transactions: Iterable[Transaction.TX]) extends Message
    case class EliminateTransactions(ids: Seq[ModifierId]) extends Message
  }
}

sealed trait GetHistoryFailure
case class GetHistoryFailureException(throwable: Throwable) extends GetHistoryFailure
sealed trait GetStateFailure
case class GetStateFailureException(throwable: Throwable) extends GetStateFailure
sealed trait GetMempoolFailure
case class GetMempoolFailureException(throwable: Throwable) extends GetMempoolFailure
sealed trait BroadcastTxFailure
case class BroadcastTxFailureException(throwable: Throwable) extends BroadcastTxFailure

trait NodeViewHolderInterface {
  def getHistory(): EitherT[Future, GetHistoryFailure, History]
  def getState(): EitherT[Future, GetStateFailure, State]
  def getMempool(): EitherT[Future, GetMempoolFailure, MemPool]
  def broadcastTransaction(tx: Transaction.TX): EitherT[Future, BroadcastTxFailure, Done.type]
}

class ActorNodeViewHolderInterface(actorRef: ActorRef)(implicit ec: ExecutionContext, timeout: Timeout)
    extends NodeViewHolderInterface {
  import cats.implicits._
  import co.topl.catsakka.CatsActor._

  override def getHistory(): EitherT[Future, GetHistoryFailure, History] =
    actorRef
      .askEither[ChangedHistory[History]](
        NodeViewHolder.ReceivableMessages.GetNodeViewChanges(history = true, state = false, mempool = false)
      )
      .leftMap { case AskException(throwable) => GetHistoryFailureException(throwable) }
      .leftMap(e => e: GetHistoryFailure)
      .map(_.reader)

  override def getState(): EitherT[Future, GetStateFailure, State] =
    actorRef
      .askEither[ChangedState[State]](
        NodeViewHolder.ReceivableMessages.GetNodeViewChanges(history = false, state = true, mempool = false)
      )
      .leftMap { case AskException(throwable) => GetStateFailureException(throwable) }
      .leftMap(e => e: GetStateFailure)
      .map(_.reader)

  override def getMempool(): EitherT[Future, GetMempoolFailure, MemPool] =
    actorRef
      .askEither[ChangedMempool[MemPool]](
        NodeViewHolder.ReceivableMessages.GetNodeViewChanges(history = false, state = false, mempool = true)
      )
      .leftMap { case AskException(throwable) => GetMempoolFailureException(throwable) }
      .leftMap(e => e: GetMempoolFailure)
      .map(_.reader)

  def broadcastTransaction(tx: Transaction.TX): EitherT[Future, BroadcastTxFailure, Done.type] =
    (actorRef ! NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction(tx))
      .asRight[BroadcastTxFailure]
      .map(_ => Done)
      .toEitherT[Future]
}
