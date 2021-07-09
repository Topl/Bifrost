package co.topl.nodeView

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.receptionist.Receptionist.Listing
import akka.actor.typed.scaladsl._
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, ActorSystem, _}
import akka.util.Timeout
import cats.data.EitherT
import co.topl.attestation.Address
import co.topl.consensus.Forger.ReceivableMessages.GenerateGenesis
import co.topl.consensus.{Forger, LocallyGeneratedModifier}
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
import co.topl.nodeView.history.{History, HistoryReader}
import co.topl.nodeView.mempool.{MemPool, MemPoolReader}
import co.topl.nodeView.state.{State, StateReader}
import co.topl.settings.{AppContext, AppSettings, NodeViewReady}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.serialization.BifrostSerializer
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object NodeViewHolder {

  case class UpdateInformation[HIS, MS, PMOD <: PersistentNodeViewModifier](
    history:                 HIS,
    state:                   MS,
    failedMod:               Option[PMOD],
    alternativeProgressInfo: Option[ProgressInfo[PMOD]],
    suffix:                  IndexedSeq[PMOD]
  )
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

private class NodeViewWriter(appContext: AppContext)(implicit
  np:                                    NetworkPrefix,
  system:                                ActorSystem[_]
) {

  import NodeViewHolder._
  import NodeViewWriter.Messages

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  lazy val modifierCompanions: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]] =
    Map(Block.modifierTypeId -> BlockSerializer, Transaction.modifierTypeId -> TransactionSerializer)

  def handle(message: NodeViewWriter.Message, nodeView: NodeView): NodeView = {
    val result =
      message match {
        case Messages.WriteBlock(block) =>
          log.info(s"Got locally generated modifier ${block.id} of type ${block.modifierTypeId}")
          pmodModify(block, nodeView)
        case Messages.WriteTransactions(transactions) =>
          transactions.foldLeft(nodeView)((view, tx) => txModify(tx, view))
        case Messages.EliminateTransactions(ids) =>
          eliminateTransactions(ids, nodeView)
      }

    system.eventStream.tell(EventStream.Publish(NodeViewChanged))

    result
  }

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

  private def pmodModify(pmod: Block, nodeView: NodeView): NodeView =
    if (!nodeView.history.contains(pmod.id)) {
      system.eventStream.tell(EventStream.Publish(StartingPersistentModifierApplication(pmod)))

      // check that the transactions are semantically valid
      if (pmod.transactions.forall(_.semanticValidation(nodeView.state).isValid)) {
        log.info(s"Apply modifier ${pmod.id} of type ${pmod.modifierTypeId} to nodeViewHolder")

        // append the block to history
        nodeView.history.append(pmod) match {
          case Success((historyBeforeStUpdate, progressInfo)) =>
            log.debug(s"Going to apply modifications to the state: $progressInfo")
            system.eventStream.tell(EventStream.Publish(SyntacticallySuccessfulModifier(pmod)))
            system.eventStream.tell(EventStream.Publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds())))

            if (progressInfo.toApply.nonEmpty) {
              val (newHistory, newStateTry, blocksApplied) =
                updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())

              newStateTry match {
                case Success(newMinState) =>
                  val newMemPool = updateMemPool(progressInfo.toRemove, blocksApplied, nodeView.mempool)
                  log.info(s"Persistent modifier ${pmod.id} applied successfully")
                  notifyHistoryUpdate()
                  notifyStateUpdate()
                  notifyMemPoolUpdate()
                  NodeView(newHistory, newMinState, newMemPool)

                case Failure(e) =>
                  log.error(s"Can`t apply persistent modifier (id: ${pmod.id}, contents: $pmod) to minimal state", e)
                  system.eventStream.tell(EventStream.Publish(SemanticallyFailedModification(pmod, e)))
                  notifyHistoryUpdate()
                  nodeView.copy(history = newHistory)
              }
            } else {
              requestDownloads(progressInfo)
              notifyHistoryUpdate()
              nodeView.copy(history = historyBeforeStUpdate)
            }
          case Failure(e) =>
            log.error(s"Can`t apply persistent modifier (id: ${pmod.id}, contents: $pmod) to history", e)
            system.eventStream.tell(EventStream.Publish(SyntacticallyFailedModification(pmod, e)))
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

  private def eliminateTransactions(ids: Seq[ModifierId], nodeView: NodeView): NodeView = {
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

private object NodeViewWriter {

  sealed abstract class Message

  object Messages {
    case class WriteBlocks(blocks: Iterable[Block]) extends Message
    case class WriteBlock(block: Block) extends Message
    case class WriteTransactions(transactions: Iterable[Transaction.TX]) extends Message
    case class EliminateTransactions(ids: Seq[ModifierId]) extends Message
  }
}

case class BroadcastTxFailure(throwable: Throwable)
case class WithNodeViewFailure(reason: Throwable)

trait NodeViewHolderInterface {
  def withNodeView[T](f:       ReadableNodeView => T): EitherT[Future, WithNodeViewFailure, T]
  def broadcastTransaction(tx: Transaction.TX): EitherT[Future, BroadcastTxFailure, Done.type]
}

class ActorNodeViewHolderInterface(actorRef: ActorRef[NodeViewReaderWriter.ReceivableMessage])(implicit
  system:                                    ActorSystem[_],
  timeout:                                   Timeout
) extends NodeViewHolderInterface {
  import akka.actor.typed.scaladsl.AskPattern._
  import cats.implicits._
  import system.executionContext

  override def withNodeView[T](f: ReadableNodeView => T): EitherT[Future, WithNodeViewFailure, T] =
    EitherT(
      actorRef
        .ask[T](NodeViewReaderWriter.ReceivableMessages.Read(f, _))
        .map(Right(_))
        .recover { case e => Left(WithNodeViewFailure(e)) }
    )

  override def broadcastTransaction(tx: Transaction.TX): EitherT[Future, BroadcastTxFailure, Done.type] =
    actorRef
      .tell(NodeViewReaderWriter.ReceivableMessages.WriteTransactions(List(tx)))
      .asRight[BroadcastTxFailure]
      .map(_ => Done)
      .toEitherT[Future]
}

case object NodeViewChanged

object NodeViewReaderWriter {

  final val ActorName = "node-view-holder"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    private[NodeViewReaderWriter] case object Initialize extends ReceivableMessage

    private[NodeViewReaderWriter] case class Initialized(
      nodeView: NodeView,
      cache:    ActorRef[NodeViewModifiersCache.ReceivableMessage]
    ) extends ReceivableMessage

    case class InitializationFailed(reason: Throwable) extends ReceivableMessage

    case class Read[T](f: ReadableNodeView => T, replyTo: ActorRef[T]) extends ReceivableMessage {

      private[NodeViewReaderWriter] def run(readableNodeView: ReadableNodeView): Unit =
        replyTo.tell(f(readableNodeView))
    }

    case class WriteBlocks(blocks: Iterable[Block]) extends ReceivableMessage

    private[NodeViewReaderWriter] case class Write(f: NodeView => NodeView, replyTo: ActorRef[NodeView])
        extends ReceivableMessage

    private[NodeViewReaderWriter] case object BlockWritten extends ReceivableMessage

    case class WriteTransactions(transactions: Iterable[Transaction.TX]) extends ReceivableMessage
    case class EvictTransactions(transactionIds: Iterable[ModifierId]) extends ReceivableMessage

    private[nodeView] case class GetWritableNodeView(replyTo: ActorRef[NodeView]) extends ReceivableMessage

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

  private def uninitialized(
    appSettings:            AppSettings,
    appContext:             AppContext
  )(implicit networkPrefix: NetworkPrefix): Behavior[ReceivableMessage] =
    Behaviors.withStash(UninitializedStashSize)(stash =>
      Behaviors.receivePartial {
        case (context, ReceivableMessages.Initialize) =>
          val cache = context.spawn(NodeViewModifiersCache(appSettings), "NodeViewModifiersCache")
          restoreState(appSettings) match {
            case Some(value) =>
              context.self.tell(ReceivableMessages.Initialized(value, cache))
            case None =>
              implicit val system: ActorSystem[_] = context.system
              implicit val timeout: Timeout = Timeout(10.seconds)
              context.pipeToSelf(genesisState(appSettings)) {
                case Success(state) =>
                  ReceivableMessages.Initialized(state, cache)
                case Failure(exception) =>
                  ReceivableMessages.InitializationFailed(exception)

              }
          }
          Behaviors.same

        case (context, ReceivableMessages.Initialized(nodeView, cache)) =>
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
    cache:          ActorRef[NodeViewModifiersCache.ReceivableMessage],
    nodeViewWriter: NodeViewWriter
  ): Behavior[ReceivableMessage] =
    Behaviors
      .receivePartial[ReceivableMessage] {
        case (_, r: ReceivableMessages.Read[_]) =>
          r.run(nodeView.readableNodeView)
          Behaviors.same

        case (_, ReceivableMessages.WriteBlocks(blocks)) =>
          cache.tell(NodeViewModifiersCache.ReceivableMessages.Insert(blocks))
          Behaviors.same

        case (_, r: ReceivableMessages.Write) =>
          val newNodeView = r.f(nodeView)
          r.replyTo.tell(newNodeView)
          initialized(newNodeView, cache, nodeViewWriter)

        case (context, ReceivableMessages.BlockWritten) =>
          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          cache.tell(
            NodeViewModifiersCache.ReceivableMessages.Pop(
              context.messageAdapter[Block](block =>
                ReceivableMessages.Write(
                  nodeViewWriter.handle(NodeViewWriter.Messages.WriteBlock(block), _),
                  context.messageAdapter[NodeView](_ => ReceivableMessages.BlockWritten)
                )
              )
            )
          )
          Behaviors.same

        case (context, ReceivableMessages.WriteTransactions(transactions)) =>
          val newNodeView =
            nodeViewWriter.handle(NodeViewWriter.Messages.WriteTransactions(transactions), nodeView)
          context.system.eventStream.tell(EventStream.Publish(NodeViewChanged))
          initialized(newNodeView, cache, nodeViewWriter)

        case (context, ReceivableMessages.EvictTransactions(transactionIds)) =>
          val newNodeView =
            nodeViewWriter.handle(NodeViewWriter.Messages.EliminateTransactions(transactionIds.toSeq), nodeView)
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

object NodeViewModifiersCache {
  import akka.actor.typed._
  import akka.actor.typed.scaladsl._

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    case class Insert(values: Iterable[Block]) extends ReceivableMessage
    case class Pop(replyTo: ActorRef[Block]) extends ReceivableMessage
  }

  implicit private val blockOrdering: Ordering[Block] =
    (x: Block, y: Block) => x.height.compare(y.height)

  private val StashSize = 100

  def apply(appSettings: AppSettings): Behavior[ReceivableMessage] = Behaviors.setup { _ =>
    var cache = mutable.PriorityQueue[Block]()

    Behaviors.withStash(StashSize)(stash =>
      Behaviors.receiveMessage {
        case ReceivableMessages.Insert(values) =>
          cache.addAll(values)
          if (cache.size > appSettings.network.maxModifiersCacheSize) {
            cache = cache.take(appSettings.network.maxModifiersCacheSize)
          }
          stash.unstashAll(Behaviors.same)
          Behaviors.same
        case m @ ReceivableMessages.Pop(replyTo) =>
          if (cache.nonEmpty) replyTo.tell(cache.dequeue())
          else stash.stash(m)
          Behaviors.same
      }
    )
  }
}

case class LocallyGeneratedTransaction(transaction: Transaction.TX)
