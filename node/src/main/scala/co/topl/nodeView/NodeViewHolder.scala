package co.topl.nodeView

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl._
import akka.actor.typed.{ActorRef, ActorSystem, _}
import akka.pattern.StatusReply
import akka.util.Timeout
import cats.data.{EitherT, Validated}
import co.topl.attestation.Address
import co.topl.consensus.KeyManager.StartupKeyView
import co.topl.consensus.{Forger, LocallyGeneratedModifier}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.ProgramId
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.validation.implicits._
import co.topl.network.NodeViewSynchronizer.ReceivableMessages._
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.history.GenericHistory.ProgressInfo
import co.topl.nodeView.history.{GenericHistory, History, HistoryReader}
import co.topl.nodeView.mempool.{MemPool, MemPoolReader, MemoryPool}
import co.topl.nodeView.state.{MinimalState, State, StateReader}
import co.topl.settings.{AppSettings, NodeViewReady}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.actors.SortedCache
import co.topl.utils.{NetworkType, TimeProvider}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
 * A Typed actor which holds onto block history, box state, and a transaction mempool.  The outside world can
 * issue mutation instructions to it, or it reader functions can be provided to the actor to read data from the view.
 */
object NodeViewHolder extends ExtensionId[NodeViewHolderInterface] {

  final val ActorName = "node-view-holder"
  final val serviceKey: ServiceKey[ReceivableMessage] = ServiceKey(ActorName)

  case class UpdateInformation(
    history:                 GenericHistory[Block, BifrostSyncInfo, History],
    state:                   MinimalState[Block, State],
    failedMod:               Option[Block],
    alternativeProgressInfo: Option[ProgressInfo[Block]],
    suffix:                  IndexedSeq[Block]
  )

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    /**
     * A self-message that's sent to the actor once async initialization is completed
     * @param nodeView the initialized NodeView
     */
    private[NodeViewHolder] case class Initialized(nodeView: NodeView) extends ReceivableMessage

    /**
     * A self-message indicating that initialization failed
     */
    private[NodeViewHolder] case class InitializationFailed(reason: Throwable) extends ReceivableMessage

    /**
     * The main public "read" interface for interacting with a read-only node view.  It accepts a function which is
     * run on a readable node view and returns some sort of data belonging to the domain of the caller.
     * @param f A function to extract data from the node view
     * @param replyTo The actor that asked for the data
     * @tparam T The caller's domain-specific response type
     */
    case class Read[T](f: ReadableNodeView => T, replyTo: ActorRef[StatusReply[T]]) extends ReceivableMessage {

      private[NodeViewHolder] def run(readableNodeView: ReadableNodeView): Unit =
        replyTo.tell(
          Try(f(readableNodeView)).fold[StatusReply[T]](e => StatusReply.error(e), StatusReply.success)
        )
    }

    /**
     * The main public "write" interface for block data.
     */
    case class WriteBlocks(blocks: Iterable[Block]) extends ReceivableMessage

    /**
     * An child-internal/private message for handling a single Block write operation at a time
     * @param f a function which persists data to the node view and returns a new node view
     * @param replyTo An actor to send the new node view to
     */
    private[NodeViewHolder] case class Write(f: NodeView => NodeView, replyTo: ActorRef[StatusReply[NodeView]])
        extends ReceivableMessage

    /**
     * A self-message indicating that a block was successfully written
     */
    private[NodeViewHolder] case object BlockWritten extends ReceivableMessage

    /**
     * A self-message indicating that a block failed to write
     */
    private[NodeViewHolder] case class BlockFailedToWrite(reason: Throwable) extends ReceivableMessage

    /**
     * Public message to write transactions
     */
    case class WriteTransactions(transactions: Iterable[Transaction.TX]) extends ReceivableMessage

    /**
     * Remove transactions from the mempool
     */
    case class EliminateTransactions(transactionIds: Iterable[ModifierId]) extends ReceivableMessage

    /**
     * A message specifically used by unit tests to get the mutable internal actor state
     */
    private[nodeView] case class GetWritableNodeView(replyTo: ActorRef[NodeView]) extends ReceivableMessage

    /**
     * A message specifically used by unit tests to mutate the internal actor state
     */
    private[nodeView] case class ModifyNodeView(f: NodeView => NodeView, replyTo: ActorRef[Done])
        extends ReceivableMessage
  }

  def apply(
    appSettings:            AppSettings,
    initialState:           () => Future[NodeView]
  )(implicit networkPrefix: NetworkPrefix, timeProvider: TimeProvider): Behavior[ReceivableMessage] =
    Behaviors.setup { context =>
      context.pipeToSelf(initialState())(
        _.fold(ReceivableMessages.InitializationFailed, ReceivableMessages.Initialized)
      )
      uninitialized
    }

  final private val UninitializedStashSize = 150

  implicit private val blockOrdering: Ordering[Block] = (a, b) => a.height.compareTo(b.height)

  /**
   * The starting state of a NodeViewHolder actor.  It does not have a NodeView yet, so something in the background should
   * be fetching a NodeView and forwarding it to this uninitialized state.
   * @return A Behavior that is uninitialized
   */
  private def uninitialized(implicit
    networkPrefix: NetworkPrefix,
    timeProvider:  TimeProvider
  ): Behavior[ReceivableMessage] =
    Behaviors.withStash(UninitializedStashSize)(stash =>
      Behaviors.receivePartial {
        case (context, ReceivableMessages.Initialized(nodeView)) =>
          implicit val system: ActorSystem[_] = context.system
          val cache =
            context.spawn(SortedCache(), "NodeViewModifiersCache")

          Receptionist(system).ref.tell(Receptionist.Register(serviceKey, context.self))
          system.eventStream.tell(EventStream.Publish(NodeViewReady(context.self)))
          system.eventStream.tell(
            EventStream.Subscribe[LocallyGeneratedModifier](
              context.messageAdapter(locallyGeneratedModifier =>
                ReceivableMessages.WriteBlocks(List(locallyGeneratedModifier.block))
              )
            )
          )
          system.eventStream.tell(
            EventStream.Subscribe[LocallyGeneratedTransaction](
              context.messageAdapter(locallyGeneratedTransaction =>
                ReceivableMessages.WriteTransactions(List(locallyGeneratedTransaction.transaction))
              )
            )
          )

          handleNextBlock(context, cache, nodeView)
          stash.unstashAll(initialized(nodeView, cache))
        case (_, ReceivableMessages.InitializationFailed(reason)) =>
          throw reason
        case (_, message) =>
          stash.stash(message)
          Behaviors.same
      }
    )

  private def initialized(
    nodeView:               NodeView,
    cache:                  ActorRef[SortedCache.ReceivableMessage[Block]]
  )(implicit networkPrefix: NetworkPrefix, timeProvider: TimeProvider): Behavior[ReceivableMessage] =
    Behaviors
      .receivePartial[ReceivableMessage] {
        case (_, r: ReceivableMessages.Read[_]) =>
          r.run(nodeView.readableNodeView)
          Behaviors.same

        case (_, ReceivableMessages.WriteBlocks(blocks)) =>
          cache.tell(SortedCache.ReceivableMessages.Insert(blocks))
          Behaviors.same

        case (_, r: ReceivableMessages.Write) =>
          Try(r.f(nodeView)) match {
            case Failure(exception) =>
              r.replyTo.tell(StatusReply.error(exception))
              Behaviors.same
            case Success(newNodeView) =>
              r.replyTo.tell(StatusReply.success(newNodeView))
              initialized(newNodeView, cache)
          }

        case (context, ReceivableMessages.BlockWritten) =>
          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          handleNextBlock(context, cache, nodeView)
          Behaviors.same

        case (context, ReceivableMessages.BlockFailedToWrite(reason)) =>
          // TODO: Should we hold onto the block in the cache?
          // TODO: Ban-list bad blocks?
          context.log.error("Failed to persist block", reason)
          handleNextBlock(context, cache, nodeView)
          Behaviors.same

        case (context, ReceivableMessages.WriteTransactions(transactions)) =>
          implicit def system: ActorSystem[_] = context.system
          val newNodeView =
            nodeView.withTransactions(transactions, eventStreamPublishMessage)
          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          initialized(newNodeView, cache)

        case (context, ReceivableMessages.EliminateTransactions(transactionIds)) =>
          implicit def system: ActorSystem[_] = context.system
          val newNodeView =
            nodeView.withoutTransactions(transactionIds.toSeq, eventStreamPublishMessage)

          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          initialized(newNodeView, cache)

        case (_, ReceivableMessages.GetWritableNodeView(replyTo)) =>
          replyTo.tell(nodeView)
          Behaviors.same

        case (_, ReceivableMessages.ModifyNodeView(f, replyTo)) =>
          replyTo.tell(Done)
          initialized(f(nodeView), cache)
      }
      .receiveSignal { case (_, PostStop) =>
        nodeView.close()
        Behaviors.same
      }

  private def eventStreamPublishMessage(implicit system: ActorSystem[_]): Any => Unit =
    (v: Any) => system.eventStream.tell(EventStream.Publish(v))

  /**
   * A recursive process which pops a block from our child cache actor, writes that block, and then repeats
   */
  private def handleNextBlock(
    context:                ActorContext[ReceivableMessage],
    cache:                  ActorRef[SortedCache.ReceivableMessage[Block]],
    nodeView:               NodeView
  )(implicit networkPrefix: NetworkPrefix, timeProvider: TimeProvider): Unit = {
    implicit def system: ActorSystem[_] = context.system
    cache.tell(
      SortedCache.ReceivableMessages.Pop(
        nodeView.history.extendsKnownTine,
        context.messageAdapter[Block](block =>
          ReceivableMessages.Write(
            _.withBlock(block, eventStreamPublishMessage),
            context.messageAdapter[StatusReply[NodeView]] {
              case StatusReply.Success(_) => ReceivableMessages.BlockWritten
              case StatusReply.Error(e)   => ReceivableMessages.BlockFailedToWrite(e)
            }
          )
        )
      )
    )
  }

  override def createExtension(system: ActorSystem[_]): NodeViewHolderInterface =
    new DelayedNodeViewHolderInterface()(system)
}

/**
 * A mutable/writable representation of our current node.
 * @param history A mutable representation of History
 * @param state A mutable representation of State
 * @param mempool A mutable representation of a Mempool
 */
case class NodeView(
  history: GenericHistory[Block, BifrostSyncInfo, History],
  state:   MinimalState[Block, State],
  mempool: MemoryPool[Transaction.TX, MemPool]
) extends AutoCloseable {

  def readableNodeView: ReadableNodeView =
    ReadableNodeView(
      history.getReader,
      state.getReader,
      mempool.getReader
    )

  override def close(): Unit = {
    history match {
      case c: AutoCloseable =>
        c.close()
      case _ =>
    }
    state match {
      case c: AutoCloseable =>
        c.close()
      case _ =>
    }
  }

  import NodeViewHolder.UpdateInformation

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  // TODO: Use a WriterT to handle system messages

  def withTransactions(transactions: Iterable[Transaction.TX], publishSystemMessage: Any => Unit)(implicit
    networkPrefix:                   NetworkPrefix,
    timeProvider:                    TimeProvider,
    system:                          ActorSystem[_]
  ): NodeView =
    transactions.foldLeft(this)(_.withTransaction(_, publishSystemMessage))

  def withTransaction(tx: Transaction.TX, publishSystemMessage: Any => Unit)(implicit
    networkPrefix:        NetworkPrefix,
    timeProvider:         TimeProvider
  ): NodeView =
    tx.syntacticValidation.toEither match {
      case Right(_) =>
        mempool.put(tx, timeProvider.time) match {
          case Success(pool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            publishSystemMessage(SuccessfulTransaction[Transaction.TX](tx))
            copy(mempool = pool)

          case Failure(e) =>
            publishSystemMessage(FailedTransaction(tx.id, e, immediateFailure = true))
            this
        }

      case Left(e) =>
        publishSystemMessage(
          FailedTransaction(tx.id, new Exception(e.head.toString), immediateFailure = true)
        )
        this
    }

  def withBlock(block: Block, publishSystemMessage: Any => Unit)(implicit
    networkPrefix:     NetworkPrefix,
    timeProvider:      TimeProvider
  ): NodeView =
    if (!history.contains(block.id)) {
      publishSystemMessage(StartingPersistentModifierApplication(block))
      import cats.implicits._

      block.transactions.traverse(_.semanticValidation(state)) match {
        case Validated.Valid(_) =>
          log.info("Applying valid blockId={} to history", block.id)

          history.append(block) match {
            case Success((historyBeforeStUpdate, progressInfo)) =>
              log.info("Block blockId={} applied to history successfully", block.id)
              log.debug("Applying valid blockId={} to state with progressInfo={}", block.id, progressInfo)
              publishSystemMessage(SyntacticallySuccessfulModifier(block))
              publishSystemMessage(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))

              if (progressInfo.toApply.nonEmpty) {
                val (newHistory, newStateTry, blocksApplied) =
                  updateState(historyBeforeStUpdate, state, progressInfo, IndexedSeq(), publishSystemMessage)

                newStateTry match {
                  case Success(newMinState) =>
                    val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, mempool)
                    log.info("Block blockId={} applied to state successfully", block.id)
                    publishSystemMessage(ChangedHistory)
                    publishSystemMessage(ChangedState)
                    publishSystemMessage(ChangedMempool)
                    NodeView(newHistory, newMinState, newMemPool)

                  case Failure(e) =>
                    log.error(s"Error applying state for blockId=${block.id} block=$block", e)
                    publishSystemMessage(SemanticallyFailedModification(block, e))
                    publishSystemMessage(ChangedHistory)
                    copy(history = newHistory)
                }
              } else {
                requestDownloads(progressInfo, publishSystemMessage)
                publishSystemMessage(ChangedHistory)
                copy(history = historyBeforeStUpdate)
              }
            case Failure(e) =>
              log.error(s"Error applying history for blockId=${block.id} block=$block", e)
              publishSystemMessage(SyntacticallyFailedModification(block, e))
              this
          }
        case Validated.Invalid(e) =>
          e.iterator.foreach(error =>
            log.warn(
              "Error applying history for blockId={} block={} due to semantic validation failure: {}",
              block.id,
              block,
              error
            )
          )
          this
      }
    } else {
      log.warn("Block with blockId={} already exists in history.  Skipping.", block.id)
      this
    }

  private def requestDownloads(pi: ProgressInfo[Block], publishSystemMessage: Any => Unit): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      publishSystemMessage(DownloadRequest(tid, id))
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
    history:              GenericHistory[Block, BifrostSyncInfo, History],
    state:                MinimalState[Block, State],
    progressInfo:         ProgressInfo[Block],
    suffixApplied:        IndexedSeq[Block],
    publishSystemMessage: Any => Unit
  ): (GenericHistory[Block, BifrostSyncInfo, History], Try[MinimalState[Block, State]], Seq[Block]) = {

    requestDownloads(progressInfo, publishSystemMessage)

    val (stateToApplyTry: Try[State], suffixTrimmed: IndexedSeq[Block]) = if (progressInfo.chainSwitchingNeeded) {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val branchingPoint = progressInfo.branchPoint.get //todo: .get
      if (state.version != branchingPoint) {
        state.rollbackTo(branchingPoint) -> trimChainSuffix(suffixApplied, branchingPoint)
      } else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        val stateUpdateInfo = applyState(history, stateToApply, suffixTrimmed, progressInfo, publishSystemMessage)

        stateUpdateInfo.failedMod match {
          case Some(_) =>
            @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
            val alternativeProgressInfo = stateUpdateInfo.alternativeProgressInfo.get
            updateState(
              stateUpdateInfo.history,
              stateUpdateInfo.state,
              alternativeProgressInfo,
              stateUpdateInfo.suffix,
              publishSystemMessage
            )
          case None => (stateUpdateInfo.history, Success(stateUpdateInfo.state), stateUpdateInfo.suffix)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        publishSystemMessage(RollbackFailed)
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  private[nodeView] def updateMemPool(
    blocksRemoved:          Seq[Block],
    blocksApplied:          Seq[Block],
    memPool:                MemoryPool[Transaction.TX, MemPool]
  )(implicit networkPrefix: NetworkPrefix, timeProvider: TimeProvider): MemPool = {
    // drop the first two transactions, since these are the reward transactions and invalid
    val rolledBackTxs = blocksRemoved.flatMap(_.transactions.drop(2))

    val appliedTxs = blocksApplied.flatMap(_.transactions)

    memPool
      .putWithoutCheck(rolledBackTxs, timeProvider.time)
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
    history:              GenericHistory[Block, BifrostSyncInfo, History],
    stateToApply:         MinimalState[Block, State],
    suffixTrimmed:        IndexedSeq[Block],
    progressInfo:         ProgressInfo[Block],
    publishSystemMessage: Any => Unit
  ): UpdateInformation = {

    val updateInfoInit = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)

    progressInfo.toApply.foldLeft(updateInfoInit) { case (updateInfo, modToApply) =>
      if (updateInfo.failedMod.isEmpty) {
        updateInfo.state.applyModifier(modToApply) match {
          case Success(stateAfterApply) =>
            val newHis = history.reportModifierIsValid(modToApply)
            publishSystemMessage(SemanticallySuccessfulModifier(modToApply))
            UpdateInformation(newHis, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)

          case Failure(e) =>
            val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            publishSystemMessage(SemanticallyFailedModification(modToApply, e))
            UpdateInformation(newHis, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      } else updateInfo
    }
  }

  def withoutTransactions(ids: Seq[ModifierId], publishSystemMessage: Any => Unit): NodeView = {
    log.debug(s"${Console.YELLOW} Removing transactions with ids: $ids from mempool${Console.RESET}")
    val updatedPool = mempool.filter(tx => !ids.contains(tx.id))
    publishSystemMessage(ChangedMempool)
    ids.foreach { id =>
      val e = new Exception("Became invalid")
      publishSystemMessage(FailedTransaction(id, e, immediateFailure = false))
    }
    copy(mempool = updatedPool)
  }
}

object NodeView {

  def persistent(
    settings:       AppSettings,
    networkType:    NetworkType,
    startupKeyView: () => Future[StartupKeyView]
  )(implicit ec:    ExecutionContext): Future[NodeView] =
    local(settings)(networkType.netPrefix).fold(genesis(settings, networkType, startupKeyView))(Future.successful)

  def local(settings: AppSettings)(implicit networkPrefix: NetworkPrefix): Option[NodeView] =
    if (State.exists(settings)) {
      Some(
        NodeView(
          History.readOrGenerate(settings),
          State.readOrGenerate(settings),
          MemPool.emptyPool
        )
      )
    } else None

  def genesis(settings: AppSettings, networkType: NetworkType, startupKeyView: () => Future[StartupKeyView])(implicit
    ec:                 ExecutionContext
  ): Future[NodeView] = {
    implicit def networkPrefix: NetworkPrefix = networkType.netPrefix
    Forger
      .genesisBlock(settings, networkType, startupKeyView)
      .map(genesis(settings, networkType, _))
  }

  def genesis(settings: AppSettings, networkType: NetworkType, genesisBlock: Block): NodeView = {
    implicit def networkPrefix: NetworkPrefix = networkType.netPrefix
    NodeView(
      History.readOrGenerate(settings).append(genesisBlock).get._1,
      State.genesisState(settings, Seq(genesisBlock)),
      MemPool.emptyPool
    )
  }
}

case class ReadableNodeView(
  history: HistoryReader[Block, BifrostSyncInfo],
  state:   StateReader[ProgramId, Address],
  memPool: MemPoolReader[Transaction.TX]
)

case class ReadFailure(reason: Throwable)
case class ApplyFailure(reason: Throwable)
case class UnapplyFailure(reason: Throwable)

trait NodeViewHolderInterface extends Extension {
  def withNodeView[T](f:                  ReadableNodeView => T): EitherT[Future, ReadFailure, T]
  def applyBlocks(blocks:                 Iterable[Block]): EitherT[Future, ApplyFailure, Done]
  def applyTransactions(tx:               Transaction.TX): EitherT[Future, ApplyFailure, Done]
  def unapplyTransactions(transactionIds: Iterable[ModifierId]): EitherT[Future, UnapplyFailure, Done]
  def onReady(): Future[Done]
}

class DelayedNodeViewHolderInterface(implicit system: ActorSystem[_]) extends NodeViewHolderInterface {
  import akka.actor.typed.scaladsl.AskPattern._
  import system.executionContext

  private val refPromise: Promise[NodeViewHolderInterface] = {
    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(2.seconds)
    Promise().completeWith(
      system.receptionist
        .ask[Receptionist.Listing](Receptionist.Subscribe(NodeViewHolder.serviceKey, _))
        .map(_.serviceInstances(NodeViewHolder.serviceKey).head)
        .map(new ActorNodeViewHolderInterface(_))
    )
  }

  override def withNodeView[T](f: ReadableNodeView => T): EitherT[Future, ReadFailure, T] =
    EitherT.liftF(refPromise.future).flatMap(_.withNodeView(f))

  override def applyBlocks(blocks: Iterable[Block]): EitherT[Future, ApplyFailure, Done] =
    EitherT.liftF(refPromise.future).flatMap(_.applyBlocks(blocks))

  override def applyTransactions(tx: TX): EitherT[Future, ApplyFailure, Done] =
    EitherT.liftF(refPromise.future).flatMap(_.applyTransactions(tx))

  override def unapplyTransactions(transactionIds: Iterable[ModifierId]): EitherT[Future, UnapplyFailure, Done] =
    EitherT.liftF(refPromise.future).flatMap(_.unapplyTransactions(transactionIds))

  override def onReady(): Future[Done] =
    refPromise.future.flatMap(_.onReady())

}

class ActorNodeViewHolderInterface(actorRef: ActorRef[NodeViewHolder.ReceivableMessage])(implicit
  system:                                    ActorSystem[_],
  timeout:                                   Timeout
) extends NodeViewHolderInterface {
  import akka.actor.typed.scaladsl.AskPattern._
  import cats.implicits._
  import system.executionContext

  override def withNodeView[T](f: ReadableNodeView => T): EitherT[Future, ReadFailure, T] =
    EitherT(
      actorRef
        .askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
        .map(Right(_))
        .recover { case e => Left(ReadFailure(e)) }
    )

  override def applyTransactions(tx: Transaction.TX): EitherT[Future, ApplyFailure, Done] =
    EitherT.pure[Future, ApplyFailure] {
      actorRef
        .tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(tx)))
      Done
    }

  override def applyBlocks(blocks: Iterable[Block]): EitherT[Future, ApplyFailure, Done] =
    EitherT.pure[Future, ApplyFailure] {
      actorRef
        .tell(NodeViewHolder.ReceivableMessages.WriteBlocks(blocks))
      Done
    }

  override def unapplyTransactions(transactionIds: Iterable[ModifierId]): EitherT[Future, UnapplyFailure, Done] =
    EitherT.pure[Future, UnapplyFailure] {
      actorRef
        .tell(NodeViewHolder.ReceivableMessages.EliminateTransactions(transactionIds))
      Done
    }

  override def onReady(): Future[Done] = {
    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.minutes)
    actorRef
      .askWithStatus[Done](NodeViewHolder.ReceivableMessages.Read(_ => Done, _))
  }
}

case object NodeViewChanged

case class LocallyGeneratedTransaction(transaction: Transaction.TX)
