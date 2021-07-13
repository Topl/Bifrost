package co.topl.nodeView

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.receptionist.Receptionist.Listing
import akka.actor.typed.scaladsl._
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, ActorSystem, _}
import akka.pattern.StatusReply
import akka.util.Timeout
import cats.data.{EitherT, Validated}
import co.topl.attestation.Address
import co.topl.consensus.Forger.ReceivableMessages.GenerateGenesis
import co.topl.consensus.{Forger, LocallyGeneratedModifier}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, PersistentNodeViewModifier}
import co.topl.modifier.box.ProgramId
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.validation.implicits._
import co.topl.network.NodeViewSynchronizer.ReceivableMessages._
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.history.GenericHistory.ProgressInfo
import co.topl.nodeView.history.{History, HistoryReader}
import co.topl.nodeView.mempool.{MemPool, MemPoolReader}
import co.topl.nodeView.state.{State, StateReader}
import co.topl.settings.{AppContext, AppSettings, NodeViewReady}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.akka.SortedCache
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

/**
 * A Typed actor which holds onto block history, box state, and a transaction mempool.  The outside world can
 * issue mutation instructions to it, or it reader functions can be provided to the actor to read data from the view.
 */
object NodeViewHolder {

  final val ActorName = "node-view-holder"

  case class UpdateInformation[HIS, MS, PMOD <: PersistentNodeViewModifier](
    history:                 HIS,
    state:                   MS,
    failedMod:               Option[PMOD],
    alternativeProgressInfo: Option[ProgressInfo[PMOD]],
    suffix:                  IndexedSeq[PMOD]
  )

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    /**
     * Instruct the actor to initialize state (read data from disk or generate genesis)
     */
    private[NodeViewHolder] case object Initialize extends ReceivableMessage

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
    private[nodeView] case class SetWritableNodeView(nodeView: NodeView, replyTo: ActorRef[Done])
        extends ReceivableMessage
  }

  def apply(
    appSettings:            AppSettings,
    appContext:             AppContext
  )(implicit networkPrefix: NetworkPrefix): Behavior[ReceivableMessage] =
    Behaviors.setup { context =>
      context.self.tell(ReceivableMessages.Initialize)
      uninitialized(appSettings, appContext)
    }

  final private val UninitializedStashSize = 150

  implicit private val blockOrdering: Ordering[Block] = (a, b) => a.height.compareTo(b.height)

  private def uninitialized(
    appSettings:            AppSettings,
    appContext:             AppContext
  )(implicit networkPrefix: NetworkPrefix): Behavior[ReceivableMessage] =
    Behaviors.withStash(UninitializedStashSize)(stash =>
      Behaviors.receivePartial {
        case (context, ReceivableMessages.Initialize) =>
          restoreState(appSettings) match {
            case Some(nodeView) =>
              context.self.tell(ReceivableMessages.Initialized(nodeView))
            case None =>
              implicit val system: ActorSystem[_] = context.system
              implicit val timeout: Timeout = Timeout(10.seconds)
              context.pipeToSelf(genesisState(appSettings)) {
                case Success(state) =>
                  ReceivableMessages.Initialized(state)
                case Failure(exception) =>
                  ReceivableMessages.InitializationFailed(exception)
              }
          }
          Behaviors.same

        case (context, ReceivableMessages.Initialized(nodeView)) =>
          val cache =
            context.spawn(SortedCache(), "NodeViewModifiersCache")
          context.system.eventStream.tell(EventStream.Publish(NodeViewReady(context.self)))
          context.system.eventStream.tell(
            EventStream.Subscribe[LocallyGeneratedModifier](
              context.messageAdapter(locallyGeneratedModifier =>
                ReceivableMessages.WriteBlocks(List(locallyGeneratedModifier.block))
              )
            )
          )
          context.system.eventStream.tell(
            EventStream.Subscribe[LocallyGeneratedTransaction](
              context.messageAdapter(locallyGeneratedTransaction =>
                ReceivableMessages.WriteTransactions(List(locallyGeneratedTransaction.transaction))
              )
            )
          )
          // A block wasn't actually written, this just kicks off the recursive operation to pop and write blocks
          context.self.tell(ReceivableMessages.BlockWritten)
          implicit val system: ActorSystem[_] = context.system
          stash.unstashAll(
            initialized(nodeView, cache, new NodeViewWriter(appContext))
          )
        case (_, ReceivableMessages.InitializationFailed(reason)) =>
          throw reason
        case (_, message) =>
          stash.stash(message)
          Behaviors.same
      }
    )

  private def initialized(
    nodeView:       NodeView,
    cache:          ActorRef[SortedCache.ReceivableMessage[Block]],
    nodeViewWriter: NodeViewWriter
  ): Behavior[ReceivableMessage] =
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
              initialized(newNodeView, cache, nodeViewWriter)
          }

        case (context, ReceivableMessages.BlockWritten) =>
          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          handleNextBlock(context, cache, nodeView, nodeViewWriter)
          Behaviors.same

        case (context, ReceivableMessages.BlockFailedToWrite(reason)) =>
          context.log.error("Failed to persist block", reason)
          handleNextBlock(context, cache, nodeView, nodeViewWriter)
          Behaviors.same

        case (context, ReceivableMessages.WriteTransactions(transactions)) =>
          val newNodeView =
            nodeViewWriter.writeTransactions(transactions, nodeView)
          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          initialized(newNodeView, cache, nodeViewWriter)

        case (context, ReceivableMessages.EliminateTransactions(transactionIds)) =>
          val newNodeView =
            nodeViewWriter.eliminateTransactions(transactionIds.toSeq, nodeView)

          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          initialized(newNodeView, cache, nodeViewWriter)

        case (_, ReceivableMessages.GetWritableNodeView(replyTo)) =>
          replyTo.tell(nodeView)
          Behaviors.same

        case (_, ReceivableMessages.SetWritableNodeView(newNodeView, replyTo)) =>
          nodeView.history.closeStorage()
          nodeView.state.closeStorage()
          replyTo.tell(Done)
          initialized(newNodeView, cache, nodeViewWriter)
      }
      .receiveSignal { case (_, PostStop) =>
        nodeView.history.closeStorage()
        nodeView.state.closeStorage()
        Behaviors.same
      }

  private def handleNextBlock(
    context:        ActorContext[ReceivableMessage],
    cache:          ActorRef[SortedCache.ReceivableMessage[Block]],
    nodeView:       NodeView,
    nodeViewWriter: NodeViewWriter
  ): Unit =
    cache.tell(
      SortedCache.ReceivableMessages.Pop(
        nodeView.history.extendsKnownTine,
        context.messageAdapter[Block](block =>
          ReceivableMessages.Write(
            nodeViewWriter.writeBlock(block, _),
            context.messageAdapter[StatusReply[NodeView]] {
              case StatusReply.Success(_) => ReceivableMessages.BlockWritten
              case StatusReply.Error(e)   => ReceivableMessages.BlockFailedToWrite(e)
            }
          )
        )
      )
    )

  private def restoreState(settings: AppSettings)(implicit networkPrefix: NetworkPrefix): Option[NodeView] =
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
  private[nodeView] def genesisState(
    settings:        AppSettings
  )(implicit system: ActorSystem[_], timeout: Timeout, networkPrefix: NetworkPrefix): Future[NodeView] = {
    import akka.actor.typed.scaladsl.AskPattern._
    import system.executionContext

    // The forger will eventually register itself with the receptionist on startup, but we may need to wait a bit first
    val forgerRefFuture =
      akka.pattern
        .retry(
          () =>
            system.receptionist
              .ask[Listing](Receptionist.Find(Forger.ForgerServiceKey, _))
              .map { case Forger.ForgerServiceKey.Listing(listings) => listings.head },
          attempts = 5,
          delay = 1.seconds
        )(executionContext, system.toClassic.scheduler)

    forgerRefFuture
      .flatMap(ref => ref.ask[Try[Block]](GenerateGenesis))
      .map {
        case Success(genesisBlock) =>
          NodeView(
            History.readOrGenerate(settings).append(genesisBlock).get._1,
            State.genesisState(settings, Seq(genesisBlock)),
            MemPool.emptyPool
          )
        case Failure(ex) =>
          throw new Error(s"${Console.RED}Failed to initialize genesis due to error${Console.RESET} $ex")
      }
  }
}

case class NodeView(history: History, state: State, mempool: MemPool) {

  def readableNodeView: ReadableNodeView =
    ReadableNodeView(
      history.getReader,
      state.getReader,
      mempool.getReader
    )
}

case class ReadableNodeView(
  history: HistoryReader[Block, BifrostSyncInfo],
  state:   StateReader[ProgramId, Address],
  memPool: MemPoolReader[Transaction.TX]
)

/**
 * A helper companion for performing the logic of the node view "write" operations
 */
private class NodeViewWriter(appContext: AppContext)(implicit
  np:                                    NetworkPrefix,
  system:                                ActorSystem[_]
) {

  import NodeViewHolder.UpdateInformation

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def writeTransactions(transactions: Iterable[Transaction.TX], nodeView: NodeView): NodeView =
    transactions.foldLeft(nodeView)((view, tx) => txModify(tx, view))

  private def txModify(tx: Transaction.TX, nodeView: NodeView): NodeView =
    tx.syntacticValidation.toEither match {
      case Right(_) =>
        nodeView.mempool.put(tx, appContext.timeProvider.time) match {
          case Success(pool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            system.eventStream.tell(EventStream.Publish(SuccessfulTransaction[Transaction.TX](tx)))
            nodeView.copy(mempool = pool)

          case Failure(e) =>
            system.eventStream.tell(EventStream.Publish(FailedTransaction(tx.id, e, immediateFailure = true)))
            nodeView
        }

      case Left(e) =>
        system.eventStream.tell(
          EventStream.Publish(
            FailedTransaction(tx.id, new Exception(e.head.toString), immediateFailure = true)
          )
        )
        nodeView
    }

  def writeBlock(block: Block, nodeView: NodeView): NodeView =
    if (!nodeView.history.contains(block.id)) {
      system.eventStream.tell(EventStream.Publish(StartingPersistentModifierApplication(block)))
      import cats.implicits._

      block.transactions.traverse(_.semanticValidation(nodeView.state)) match {
        case Validated.Valid(_) =>
          log.info("Applying valid blockId={} to history", block.id)

          nodeView.history.append(block) match {
            case Success((historyBeforeStUpdate, progressInfo)) =>
              log.info("Block blockId={} applied to history successfully", block.id)
              log.debug("Applying valid blockId={} to state with progressInfo={}", block.id, progressInfo)
              system.eventStream.tell(EventStream.Publish(SyntacticallySuccessfulModifier(block)))
              system.eventStream.tell(EventStream.Publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds())))

              if (progressInfo.toApply.nonEmpty) {
                val (newHistory, newStateTry, blocksApplied) =
                  updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())

                newStateTry match {
                  case Success(newMinState) =>
                    val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, nodeView.mempool)
                    log.info("Block blockId={} applied to state successfully", block.id)
                    notifyHistoryUpdate()
                    notifyStateUpdate()
                    notifyMemPoolUpdate()
                    NodeView(newHistory, newMinState, newMemPool)

                  case Failure(e) =>
                    log.error(s"Error applying state for blockId=${block.id} block=$block", e)
                    system.eventStream.tell(EventStream.Publish(SemanticallyFailedModification(block, e)))
                    notifyHistoryUpdate()
                    nodeView.copy(history = newHistory)
                }
              } else {
                requestDownloads(progressInfo)
                notifyHistoryUpdate()
                nodeView.copy(history = historyBeforeStUpdate)
              }
            case Failure(e) =>
              log.error(s"Error applying history for blockId=${block.id} block=$block", e)
              system.eventStream.tell(EventStream.Publish(SyntacticallyFailedModification(block, e)))
              nodeView
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
          nodeView
      }
    } else {
      log.warn("Block with blockId={} already exists in history.  Skipping.", block.id)
      nodeView
    }

  private def requestDownloads(pi: ProgressInfo[Block]): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      system.eventStream.tell(EventStream.Publish(DownloadRequest(tid, id)))
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
        system.eventStream.tell(EventStream.Publish(RollbackFailed))
        //todo: what to return here? the situation is totally wrong
        ???
    }
  }

  private def notifyHistoryUpdate(): Unit =
    system.eventStream.tell(EventStream.Publish(ChangedHistory))

  private def notifyStateUpdate(): Unit =
    system.eventStream.tell(EventStream.Publish(ChangedState))

  private def notifyMemPoolUpdate(): Unit =
    system.eventStream.tell(EventStream.Publish(ChangedMempool))

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  private[nodeView] def updateMemPool(
    blocksRemoved: Seq[Block],
    blocksApplied: Seq[Block],
    memPool:       MemPool
  ): MemPool = {
    // drop the first two transactions, since these are the reward transactions and invalid
    val rolledBackTxs = blocksRemoved.flatMap(_.transactions.drop(2))

    val appliedTxs = blocksApplied.flatMap(_.transactions)

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
            system.eventStream.tell(EventStream.Publish(SemanticallySuccessfulModifier(modToApply)))
            UpdateInformation(newHis, stateAfterApply, None, None, updateInfo.suffix :+ modToApply)

          case Failure(e) =>
            val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
            system.eventStream.tell(EventStream.Publish(SemanticallyFailedModification(modToApply, e)))
            UpdateInformation(newHis, updateInfo.state, Some(modToApply), Some(newProgressInfo), updateInfo.suffix)
        }
      } else updateInfo
    }
  }

  def eliminateTransactions(ids: Seq[ModifierId], nodeView: NodeView): NodeView = {
    log.debug(s"${Console.YELLOW} Removing transactions with ids: $ids from mempool${Console.RESET}")
    val updatedPool = nodeView.mempool.filter(tx => !ids.contains(tx.id))
    notifyMemPoolUpdate()
    ids.foreach { id =>
      val e = new Exception("Became invalid")
      system.eventStream.tell(EventStream.Publish(FailedTransaction(id, e, immediateFailure = false)))
    }
    nodeView.copy(mempool = updatedPool)
  }
}

case class BroadcastTxFailure(throwable: Throwable)
case class WithNodeViewFailure(reason: Throwable)

trait NodeViewHolderInterface {
  def withNodeView[T](f:       ReadableNodeView => T): EitherT[Future, WithNodeViewFailure, T]
  def broadcastTransaction(tx: Transaction.TX): EitherT[Future, BroadcastTxFailure, Done.type]
}

class ActorNodeViewHolderInterface(actorRef: ActorRef[NodeViewHolder.ReceivableMessage])(implicit
  system:                                    ActorSystem[_],
  timeout:                                   Timeout
) extends NodeViewHolderInterface {
  import akka.actor.typed.scaladsl.AskPattern._
  import cats.implicits._
  import system.executionContext

  override def withNodeView[T](f: ReadableNodeView => T): EitherT[Future, WithNodeViewFailure, T] =
    EitherT(
      actorRef
        .askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
        .map(Right(_))
        .recover { case e => Left(WithNodeViewFailure(e)) }
    )

  override def broadcastTransaction(tx: Transaction.TX): EitherT[Future, BroadcastTxFailure, Done.type] =
    actorRef
      .tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(tx)))
      .asRight[BroadcastTxFailure]
      .map(_ => Done)
      .toEitherT[Future]
}

case object NodeViewChanged

case class LocallyGeneratedTransaction(transaction: Transaction.TX)
