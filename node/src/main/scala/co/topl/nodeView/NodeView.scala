package co.topl.nodeView

import akka.actor.typed.ActorSystem
import cats.data.{EitherT, Validated, Writer}
import cats.implicits._
import co.topl.attestation.Address
import co.topl.consensus.Hiccups.HiccupBlock
import co.topl.consensus.KeyManager.StartupKeyView
import co.topl.consensus._
import co.topl.modifier.{ModifierId, ProgramId}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.transaction.validation.implicits._
import co.topl.network.BifrostSyncInfo
import co.topl.nodeView.history.GenericHistory.ProgressInfo
import co.topl.nodeView.history.{GenericHistory, History, HistoryReader}
import co.topl.nodeView.mempool.{MemPool, MemPoolReader, MemoryPool}
import co.topl.nodeView.state.{MinimalState, State, StateReader}
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.TimeProvider
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * A mutable/writable representation of our current node.
 *
 * @param history A mutable representation of History
 * @param state A mutable representation of State
 * @param mempool A mutable representation of a Mempool
 */
case class NodeView(
  history: GenericHistory[Block, BifrostSyncInfo, History],
  state:   MinimalState[Block, State],
  mempool: MemoryPool[Transaction.TX, MemPool]
) extends NodeViewBlockOps
    with NodeViewTransactionOps
    with AutoCloseable {

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

  protected val log: Logger = LoggerFactory.getLogger(this.getClass)

  protected[nodeView] def updateMemPool(
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
}

object NodeView {

  def persistent(
    settings:           AppSettings,
    consensusInterface: ConsensusInterface,
    startupKeyView:     () => Future[StartupKeyView]
  )(implicit
    system:            ActorSystem[_],
    ec:                ExecutionContext,
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner
  ): Future[NodeView] =
    if (State.exists(settings)) {
      resume(settings)
    } else
      fetchAndApplyGenesis(settings, consensusInterface, startupKeyView)
        .valueOrF(e => Future.failed(new IllegalArgumentException(e.toString)))

  private def resume(
    settings: AppSettings
  )(implicit
    system:            ActorSystem[_],
    ec:                ExecutionContext,
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner
  ): Future[NodeView] =
    Future.successful(
      NodeView(
        History.readOrGenerate(settings),
        State.readOrGenerate(settings),
        MemPool.empty()
      )
    )

  private def fetchAndApplyGenesis(
    settings:           AppSettings,
    consensusInterface: ConsensusInterface,
    startupKeyView:     () => Future[StartupKeyView]
  )(implicit
    system:            ActorSystem[_],
    ec:                ExecutionContext,
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner
  ): EitherT[Future, NodeViewHolderInterface.ApplyFailure, NodeView] = for {
    nodeAddresses <- EitherT.liftF(startupKeyView()).map(_.addresses)
    genesis <- new GenesisProvider(protocolVersioner.applicable(1).blockVersion, nodeAddresses)
      .fetchGenesis(settings)
      .toEitherT[Future]
      .leftMap(e => NodeViewHolderInterface.ApplyFailure(new IllegalArgumentException(e.toString)))
    _ <- consensusInterface
      .update(
        genesis.block.id,
        NxtConsensus
          .StateUpdate(Some(genesis.state.totalStake), Some(genesis.block.difficulty), None, Some(genesis.block.height))
      )
      .leftMap(e => NodeViewHolderInterface.ApplyFailure(new IllegalArgumentException(e.toString)))
    nodeView = NodeView(
      History.readOrGenerate(settings).append(genesis.block, Seq()).get._1, // no validators because genesis
      State.genesisState(settings, Seq(genesis.block)),
      MemPool.empty()
    )
  } yield nodeView
}

trait NodeViewBlockOps {
  self: NodeView =>

  import NodeViewHolder.UpdateInformation

  def withBlock(block: Block, validators: Seq[BlockValidator[_]])(implicit
    networkPrefix:     NetworkPrefix,
    timeProvider:      TimeProvider
  ): Writer[List[Any], NodeView] = {
    import cats.implicits._
    if (!history.contains(block.id)) {
      Writer[List[Any], NodeView](List(NodeViewHolder.Events.StartingPersistentModifierApplication(block)), this)
        .flatMap { nodeView =>
          val semanticallyValidated =
            if (Hiccups.semanticValidation.contains(HiccupBlock(block))) {
              log.info("Skipping semantic validation for HiccupBlock.  blockId={}", block.id)
              block.transactions.validNec
            } else {
              block.transactions.traverse(_.semanticValidation(nodeView.state))
            }

          semanticallyValidated match {
            case Validated.Valid(a) =>
              log.info("Applying valid blockId={} to history", block.id)
              val openSurfaceIdsBeforeUpdate = history.openSurfaceIds()

              history.append(block, validators) match {
                case Success((historyBeforeStUpdate, progressInfo)) =>
                  log.info("Block blockId={} applied to history successfully", block.id)
                  log.debug("Applying valid blockId={} to state with progressInfo={}", block.id, progressInfo)
                  Writer[List[Any], NodeView](
                    List(
                      NodeViewHolder.Events.SyntacticallySuccessfulModifier(block),
                      NodeViewHolder.Events.NewOpenSurface(openSurfaceIdsBeforeUpdate),
                      NodeViewHolder.Events.ChangedHistory
                    ),
                    nodeView
                  )
                    .flatMap(nodeView =>
                      if (progressInfo.toApply.nonEmpty) {
                        nodeView
                          .updateState(historyBeforeStUpdate, state, progressInfo, IndexedSeq())
                          .flatMap { case (newHistory, newStateTry, blocksApplied) =>
                            newStateTry match {
                              case Success(newMinState) =>
                                val newMemPool = nodeView.updateMemPool(progressInfo.toRemove, blocksApplied, mempool)
                                log.info("Block blockId={} applied to state successfully", block.id)
                                Writer[List[Any], NodeView](
                                  List(NodeViewHolder.Events.ChangedState, NodeViewHolder.Events.ChangedMempool),
                                  NodeView(newHistory, newMinState, newMemPool)
                                )

                              case Failure(e) =>
                                log.error(s"Error applying state for blockId=${block.id} block=$block", e)
                                Writer[List[Any], NodeView](
                                  List(NodeViewHolder.Events.SemanticallyFailedModification(block, e)),
                                  nodeView.copy(history = newHistory)
                                )
                            }
                          }
                      } else {
                        requestDownloads(progressInfo)
                          .map(_ => nodeView.copy(history = historyBeforeStUpdate))
                      }
                    )
                case Failure(e) =>
                  log.error(s"Error applying history for blockId=${block.id} block=$block", e)
                  Writer[List[Any], NodeView](
                    List(NodeViewHolder.Events.SyntacticallyFailedModification(block, e)),
                    nodeView
                  )
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
              Writer.value[List[Any], NodeView](nodeView)
          }
        }
    } else {
      log.warn(s"Block with id=${block.id} already exists in history.  Skipping.", block.id)
      Writer.value[List[Any], NodeView](this)
    }
  }

  private def requestDownloads(pi: ProgressInfo[Block]): Writer[List[Any], Unit] =
    Writer.tell[List[Any]](pi.toDownload.toList.map((NodeViewHolder.Events.DownloadRequest.apply _).tupled))

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
  def updateState(
    history:       GenericHistory[Block, BifrostSyncInfo, History],
    state:         MinimalState[Block, State],
    progressInfo:  ProgressInfo[Block],
    suffixApplied: IndexedSeq[Block]
  ): Writer[List[Any], (GenericHistory[Block, BifrostSyncInfo, History], Try[MinimalState[Block, State]], Seq[Block])] =
    requestDownloads(progressInfo)
      .map(_ => this)
      .flatMap { nodeView =>
        val (stateToApplyTry, suffixTrimmed) =
          if (progressInfo.chainSwitchingNeeded) {
            @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
            val branchingPoint = progressInfo.branchPoint.get // todo: .get
            if (state.version != branchingPoint) {
              state.rollbackTo(branchingPoint) -> trimChainSuffix(suffixApplied, branchingPoint)
            } else {
              Success(state) -> IndexedSeq()
            }
          } else {
            Success(state) -> suffixApplied
          }

        stateToApplyTry match {
          case Success(stateToApply) =>
            nodeView
              .applyState(history, stateToApply, suffixTrimmed, progressInfo)
              .flatMap(stateUpdateInfo =>
                stateUpdateInfo.failedMod match {
                  case Some(_) =>
                    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
                    val alternativeProgressInfo = stateUpdateInfo.alternativeProgressInfo.get
                    nodeView.updateState(
                      stateUpdateInfo.history,
                      stateUpdateInfo.state,
                      alternativeProgressInfo,
                      stateUpdateInfo.suffix
                    )
                  case None =>
                    Writer.value((stateUpdateInfo.history, Success(stateUpdateInfo.state), stateUpdateInfo.suffix))
                }
              )
          case Failure(e) =>
            log.error("Rollback failed: ", e)
            Writer.tell(List(NodeViewHolder.Events.RollbackFailed))
            // todo: what to return here? the situation is totally wrong
            ???
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
  def applyState(
    history:       GenericHistory[Block, BifrostSyncInfo, History],
    stateToApply:  MinimalState[Block, State],
    suffixTrimmed: IndexedSeq[Block],
    progressInfo:  ProgressInfo[Block]
  ): Writer[List[Any], UpdateInformation] =
    Writer
      .value[List[Any], UpdateInformation](UpdateInformation(history, stateToApply, None, None, suffixTrimmed))
      .flatMap(updateInfoInit =>
        progressInfo.toApply.foldLeft(Writer.value[List[Any], UpdateInformation](updateInfoInit)) {
          case (updateInfoWriter, modToApply) =>
            if (updateInfoWriter.value.failedMod.isEmpty) {
              updateInfoWriter.value.state.applyModifier(modToApply) match {
                case Success(stateAfterApply) =>
                  val newHis = history.reportModifierIsValid(modToApply)
                  Writer(
                    List(NodeViewHolder.Events.SemanticallySuccessfulModifier(modToApply)),
                    UpdateInformation(newHis, stateAfterApply, None, None, updateInfoWriter.value.suffix :+ modToApply)
                  )

                case Failure(e) =>
                  val (newHis, newProgressInfo) = history.reportModifierIsInvalid(modToApply, progressInfo)
                  Writer(
                    List(NodeViewHolder.Events.SemanticallyFailedModification(modToApply, e)),
                    UpdateInformation(
                      newHis,
                      updateInfoWriter.value.state,
                      Some(modToApply),
                      Some(newProgressInfo),
                      updateInfoWriter.value.suffix
                    )
                  )
              }
            } else updateInfoWriter
        }
      )
}

trait NodeViewTransactionOps {
  self: NodeView =>

  def withTransactions(transactions: Iterable[Transaction.TX])(implicit
    networkPrefix:                   NetworkPrefix,
    timeProvider:                    TimeProvider,
    system:                          ActorSystem[_]
  ): Writer[List[Any], NodeView] =
    transactions.foldLeft(Writer(Nil: List[Any], self)) { case (writer, tx) => writer.flatMap(_.withTransaction(tx)) }

  def withTransaction(tx: Transaction.TX)(implicit
    networkPrefix:        NetworkPrefix,
    timeProvider:         TimeProvider
  ): Writer[List[Any], NodeView] =
    tx.syntacticValidation.toEither match {
      case Right(_) =>
        mempool.put(tx, timeProvider.time) match {
          case Success(pool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            Writer(List(NodeViewHolder.Events.SuccessfulTransaction[Transaction.TX](tx)), copy(mempool = pool))

          case Failure(e) =>
            Writer(List(NodeViewHolder.Events.FailedTransaction(tx.id, e, immediateFailure = true)), this)
        }

      case Left(e) =>
        Writer(
          List(NodeViewHolder.Events.FailedTransaction(tx.id, new Exception(e.head.toString), immediateFailure = true)),
          this
        )
    }

  def withoutTransactions(ids: Seq[ModifierId]): Writer[List[Any], NodeView] = {
    log.debug(s"${Console.YELLOW} Removing transactions with ids: $ids from mempool${Console.RESET}")
    val updatedPool = mempool.filter(tx => !ids.contains(tx.id))
    val e = new Exception("Became invalid")
    Writer(
      List(NodeViewHolder.Events.ChangedMempool) ++ ids.map(
        NodeViewHolder.Events.FailedTransaction(_, e, immediateFailure = false)
      ),
      copy(mempool = updatedPool)
    )
  }
}

case class ReadableNodeView(
  history: HistoryReader[Block, BifrostSyncInfo],
  state:   StateReader[ProgramId, Address],
  memPool: MemPoolReader[Transaction.TX]
)
