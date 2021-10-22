package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.data.OptionT
import cats.implicits._
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.ChainReplicatorSettings
import co.topl.tool.Exporter.txFormat
import co.topl.tools.exporter.DataType
import io.circe.syntax.EncoderOps
import org.mongodb.scala.BulkWriteResult
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * Chain replicator listens to info about new blocks from NodeViewHolder and updates the AppView with new blocks
 * It could also find the previously missing blocks and send them to the AppView
 */
object ChainReplicator {

  val actorName = "ChainReplicator"

  val stashSize = 20000

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class GotNewBlock(block: Block) extends ReceivableMessage

    private[nodeView] case class CheckMissingBlocks(maxHeight: Long) extends ReceivableMessage

    private[nodeView] case object CheckMissingBlocksDone extends ReceivableMessage

    private[nodeView] case object CheckDatabaseComplete extends ReceivableMessage

    private[nodeView] case class Terminate(reason: Throwable) extends ReceivableMessage
  }

  def apply(
    nodeViewHolderRef:    ActorRef[NodeViewHolder.ReceivableMessage],
    checkDBConnection:    () => Future[Seq[String]],
    getExistingHeightsDB: (Long, Long) => Future[Seq[Long]],
    replaceInsertDB:      (Seq[(String, String)], String, DataType) => Future[BulkWriteResult],
    settings:             ChainReplicatorSettings
  ): Behavior[ReceivableMessage] =
    Behaviors.withStash(stashSize) { buffer =>
      Behaviors.setup { implicit context =>
        implicit val ec: ExecutionContext = context.executionContext

        context.system.eventStream.tell(
          EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
            context.messageAdapter(block => ReceivableMessages.GotNewBlock(block.modifier))
          )
        )

        context.log.info(s"${Console.GREEN}Chain replicator initializing${Console.RESET}")

        context.pipeToSelf(checkDBConnection()) {
          case Success(result) =>
            context.log.info(s"${Console.GREEN}Found collections in database: $result${Console.RESET}")
            ReceivableMessages.CheckDatabaseComplete
          case Failure(e) => ReceivableMessages.Terminate(e)
        }

        new ChainReplicator(nodeViewHolderRef, buffer, getExistingHeightsDB, replaceInsertDB, settings).uninitialized

      }
    }
}

private class ChainReplicator(
  nodeViewHolderRef:    ActorRef[NodeViewHolder.ReceivableMessage],
  buffer:               StashBuffer[ChainReplicator.ReceivableMessage],
  getExistingHeightsDB: (Long, Long) => Future[Seq[Long]],
  replaceInsertDB:      (Seq[(String, String)], String, DataType) => Future[BulkWriteResult],
  settings:             ChainReplicatorSettings
)(implicit
  context: ActorContext[ChainReplicator.ReceivableMessage]
) {
  implicit private val log: Logger = context.log

  import ChainReplicator._
  import co.topl.tool.Exporter.blockEncoder

  /** The actor waits for the database check to complete, stops if chain repliactor is turned off in settings */
  def uninitialized: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.CheckDatabaseComplete =>
        log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
        if (settings.checkMissingBlock) {
          val bestBlockHeight = withNodeView(getBestBlockHeight)
          context.pipeToSelf(bestBlockHeight) {
            case Success(height) => ReceivableMessages.CheckMissingBlocks(height)
            case Failure(e)      => ReceivableMessages.Terminate(e)
          }
          log.info(s"${Console.GREEN}Chain replicator transitioning to syncing${Console.RESET}")
          buffer.unstashAll(syncing(0, 0))
        } else {
          log.info(s"${Console.GREEN}Chain replicator transitioning to listening for new blocks${Console.RESET}")
          buffer.unstashAll(listening)
        }

      case ReceivableMessages.Terminate(reason) =>
        throw reason

      case other =>
        buffer.stash(other)
        Behaviors.same
    }

  /** Find and send missing blocks to AppView, and transition to listening for new blocks once all checks are done */
  private def syncing(
    count:       Long,
    totalChecks: Long
  ): Behavior[ReceivableMessage] =
    Behaviors.receivePartial[ReceivableMessage] {
      case (context, ReceivableMessages.CheckMissingBlocks(maxHeight)) =>
        implicit val ec: ExecutionContext = context.executionContext

        lazy val checkRange = (settings.checkMissingStartHeight to maxHeight)
          .grouped(settings.blockCheckSize)
          .toList
          .map(group => (group.head, group.last))

        checkRange.foreach { case (startHeight, endHeight) =>
          val existingHeightsFuture = getExistingHeightsDB(startHeight, endHeight)
          val exportResult: OptionT[Future, (BulkWriteResult, BulkWriteResult)] = for {
            existingHeights  <- OptionT[Future, Seq[Long]](existingHeightsFuture.map(_.some))
            blocksToAdd      <- OptionT[Future, Seq[Block]](missingBlocks(startHeight, endHeight, existingHeights))
            blockWriteResult <- OptionT[Future, BulkWriteResult](exportBlocks(blocksToAdd).map(_.some))
            txWriteResult    <- OptionT[Future, BulkWriteResult](exportTransactions(blocksToAdd).map(_.some))
          } yield (blockWriteResult, txWriteResult)

          context.pipeToSelf(exportResult.value) {
            case Success(Some(writeResults)) =>
              log.info(
                s"${Console.GREEN}Successfully inserted ${writeResults._1.getUpserts.size()} blocks " +
                s"and ${writeResults._2.getUpserts.size()} transactions into AppView${Console.RESET}"
              )
              ReceivableMessages.CheckMissingBlocksDone
            case Failure(err) =>
              ReceivableMessages.Terminate(err)
            case _ =>
              log.info(s"${Console.GREEN}No missing blocks found between $startHeight and $endHeight${Console.RESET}")
              ReceivableMessages.CheckMissingBlocksDone
          }
        }
        syncing(count, checkRange.size)

      case (_, ReceivableMessages.CheckMissingBlocksDone) =>
        val newCount = count + 1
        log.info(s"${Console.GREEN}Done with $newCount/$totalChecks of the checks${Console.RESET}")
        if (newCount >= totalChecks)
          buffer.unstashAll(listening)
        else
          syncing(newCount, totalChecks)

      case (_, ReceivableMessages.Terminate(reason)) =>
        throw reason

      case (_, other) =>
        buffer.stash(other)
        Behaviors.same
    }

  /**
   * The actor exports new blocks from NodeViewHolder
   * If checkMissingBlock is turned on, start finding and exporting blocks at missing heights found in a height range
   * in the database after set amount of time
   */
  private def listening: Behavior[ReceivableMessage] =
    Behaviors.receivePartial[ReceivableMessage] {
      case (context, ReceivableMessages.GotNewBlock(block)) =>
        implicit val ec: ExecutionContext = context.executionContext
        val res = for {
          blockExport <- exportBlocks(Seq(block))
          txExport    <- exportTransactions(Seq(block))
        } yield (blockExport, txExport)
        res.onComplete {
          case Success(writeResults) =>
            log.info(
              s"${Console.GREEN}Added a block and ${writeResults._2.getUpserts.size()} " +
              s"transactions to appView at height: ${block.height}${Console.RESET}"
            )
          case Failure(err) =>
            log.error(s"${Console.RED}$err${Console.RESET}")
        }
        Behaviors.same

      case (_, ReceivableMessages.Terminate(reason)) =>
        throw reason
    }

  /**
   * Export a sequence of blocks to the AppView
   * @param blocks sequence of blocks to be inserted in AppView
   * @return Insertion result from the database
   */
  private def exportBlocks(blocks: Seq[Block]): Future[BulkWriteResult] = {
    implicit val ec: ExecutionContext = context.executionContext
    val blocksString = blocks.map(_.asJson(blockEncoder).toString)
    val blocksId = blocks.map(_.id.toString)
    replaceInsertDB(blocksId.zip(blocksString), "id", DataType.Block)
  }

  /**
   * Export transactions from a sequence of blocks to the AppView
   * @param blocks sequence of blocks that need transactions inserted in AppView
   * @return Insertion result from the database
   */
  private def exportTransactions(blocks: Seq[Block]): Future[BulkWriteResult] = {
    implicit val ec: ExecutionContext = context.executionContext
    val txString = blocks.flatMap { b =>
      b.transactions.map { tx =>
        (tx.id.toString, txFormat(b, tx).toString)
      }
    }
    replaceInsertDB(txString, "txId", DataType.Transaction)
  }

  /**
   * Given the height range of the database check, and the heights where at least one block exists in range, return the
   * blocks at missing heights from NodeView
   * @param startHeight the start height of the database check
   * @param existingHeights the sequence of heights where at least one block exists in the AppView
   * @return optional sequence of blocks that are missing in the AppView
   */
  private def missingBlocks(
    startHeight:     Long,
    endHeight:       Long,
    existingHeights: Seq[Long]
  ): Future[Option[Seq[Block]]] = {
    val exisitingHeightsSet = existingHeights.toSet
    val missingBlockHeights = (startHeight to endHeight).filterNot(exisitingHeightsSet.contains)
    withNodeView(blocksAtHeight(missingBlockHeights)(_))
  }

  /**
   * Gets the height of the best block
   * @param nodeView NodeView
   * @return height of the current best block
   */
  private def getBestBlockHeight(nodeView: ReadableNodeView): Long =
    nodeView.history.height

  /**
   * Access the NodeView and gets the blocks at given heights
   * @param blockHeights the heights of the blocks needed
   * @param nodeView NodeView
   * @return optional sequence of blocks at given heights
   */
  private def blocksAtHeight(blockHeights: Seq[Long])(nodeView: ReadableNodeView): Option[Seq[Block]] = {
    // TODO: Jing - add function in nodeView.history to get multiple modifier at heights
    val blocks = blockHeights.flatMap(nodeView.history.modifierByHeight)
    if (blocks.nonEmpty) blocks.some
    else None
  }

  /**
   * Function for accessing the nodeView
   * @param f function to be applied on nodeView
   * @tparam T result type
   * @return result from nodeView
   */
  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: ActorSystem[_] = context.system
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }
}
