package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.data.OptionT
import cats.implicits._
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.ChainReplicatorSettings
import co.topl.tools.exporter.{DataType, MongoExport}
import io.circe.syntax.EncoderOps
import org.mongodb.scala.result.InsertManyResult
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * Chain replicator listens to info about new blocks from NodeViewHolder and updates the AppView with new blocks
 * It could also find the previously missing blocks and send them to the AppView
 */
object ChainReplicator {

  val actorName = "ChainReplicator"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class GotNewBlock(block: Block) extends ReceivableMessage

    private[nodeView] case class CheckMissingBlocks(maxHeight: Long) extends ReceivableMessage

    private[nodeView] case object CheckDatabaseComplete extends ReceivableMessage

    private[nodeView] case class Terminate(reason: Throwable) extends ReceivableMessage
  }

  def apply(
    nodeViewHodlerRef: ActorRef[NodeViewHolder.ReceivableMessage],
    settings:          ChainReplicatorSettings
  ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter(block => ReceivableMessages.GotNewBlock(block.modifier))
        )
      )

      context.log.info(s"${Console.GREEN}Chain replicator initializing${Console.RESET}")

      val mongo =
        MongoExport(
          settings.uri.getOrElse("mongodb://localhost"),
          settings.database.getOrElse("bifrost"),
          settings.collection.getOrElse("blocks"),
          DataType.Block
        )

      context.pipeToSelf(mongo.checkValidConnection()) {
        case Success(result) =>
          context.log.info(s"${Console.GREEN}Found collections in database: $result${Console.RESET}")
          ReceivableMessages.CheckDatabaseComplete
        case Failure(e) => ReceivableMessages.Terminate(e)
      }

      Behaviors.withTimers(timers => new ChainReplicator(mongo, nodeViewHodlerRef, settings).uninitialized(timers))
    }
}

private class ChainReplicator(
  mongo:             MongoExport,
  nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage],
  settings:          ChainReplicatorSettings
)(implicit
  context: ActorContext[ChainReplicator.ReceivableMessage]
) {
  implicit private val log: Logger = context.log

  import ChainReplicator._
  import co.topl.tool.Exporter.blockEncoder

  /** The actor waits for the database check to complete, stops if chain repliactor is turned off in settings */
  def uninitialized(timers: TimerScheduler[ReceivableMessage]): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.CheckDatabaseComplete =>
        log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
        active(settings.checkMissingStartHeight, timers)

      case ReceivableMessages.Terminate(reason) =>
        timers.cancelAll()
        throw reason
    }

  /**
   * The actor exports new blocks from NodeViewHolder
   * If checkMissingBlock is turned on, start finding and exporting blocks at missing heights found in a height range
   * in the database after set amount of time
   * @param blockCheckStart the height at which the new round of CheckMissingBlocks starts
   */
  private def active(blockCheckStart: Long, timers: TimerScheduler[ReceivableMessage]): Behavior[ReceivableMessage] =
    Behaviors.receivePartial[ReceivableMessage] {
      case (context, ReceivableMessages.GotNewBlock(block)) =>
        implicit val ec: ExecutionContext = context.executionContext
        if (settings.checkMissingBlock)
          timers.startSingleTimer(ReceivableMessages.CheckMissingBlocks(block.height), settings.checkMissingDelay)
        exportBlocks(Seq(block)).onComplete {
          case Success(_) => log.info(s"${Console.GREEN}Inserted new block at height: ${block.height}${Console.RESET}")
          case Failure(err) => log.error(s"${Console.RED}$err${Console.RESET}")
        }
        Behaviors.same

      case (context, ReceivableMessages.CheckMissingBlocks(maxHeight)) =>
        implicit val ec: ExecutionContext = context.executionContext
        val blockCheckEnd: Long = blockCheckStart + settings.numberOfBlocksToCheck
        if (maxHeight >= blockCheckEnd) {
          val existingHeightsFuture = mongo.getExistingHeights(blockCheckStart, blockCheckEnd)
          val result: OptionT[Future, InsertManyResult] = for {
            existingHeights  <- OptionT[Future, Seq[Long]](existingHeightsFuture.map(_.some))
            blocksToAdd      <- OptionT[Future, Seq[Block]](missingBlocks(blockCheckStart, existingHeights))
            insertManyResult <- OptionT[Future, InsertManyResult](exportBlocks(blocksToAdd).map(_.some))
          } yield insertManyResult
          result.value.onComplete {
            case Success(Some(x)) =>
              log.info(
                s"${Console.GREEN}Successfully inserted ${x.getInsertedIds.size} blocks into AppView${Console.RESET}"
              )
            case Failure(err) => log.error(s"${Console.RED}$err${Console.RESET}")
            case _            => log.info(s"${Console.GREEN}No missing blocks found${Console.RESET}")
          }
          active(blockCheckEnd, timers)
        } else {
          Behaviors.same
        }

      case (_, ReceivableMessages.Terminate(reason)) =>
        timers.cancelAll()
        throw reason
    }

  /**
   * Export a sequence of blocks to the AppView
   * @param blocks sequence  of block to be inserted in AppView
   * @return Insertion result from the database
   */
  private def exportBlocks(blocks: Seq[Block]): Future[InsertManyResult] = {
    implicit val ec: ExecutionContext = context.executionContext
    val blocksString = blocks.map(_.asJson(blockEncoder).toString)
    mongo.insert(blocksString)
  }

  /**
   * Given the height range of the database check, and the heights where at least one block exists in range, return the
   * blocks at missing heights from NodeView
   * @param blockCheckStart the start height of the database check
   * @param existingHeights the sequence of heights where at least one block exists in the AppView
   * @return optional sequence of blocks that are missing in the AppView
   */
  private def missingBlocks(
    blockCheckStart: Long,
    existingHeights: Seq[Long]
  ): Future[Option[Seq[Block]]] = {
    val exisitingHeightsSet = existingHeights.toSet
    val blockCheckEnd: Long = blockCheckStart + settings.numberOfBlocksToCheck
    val missingBlockHeights = (blockCheckStart to blockCheckEnd).filterNot(exisitingHeightsSet.contains)
    withNodeView(blocksAtHeight(missingBlockHeights)(_))
  }

  /**
   * Access the NodeView and gets the blocks at given heights
   * @param blockHeights the heights of the blocks needed
   * @param nodeView NodeView
   * @return optional sequence of blocks at given heights
   */
  private def blocksAtHeight(
    blockHeights: Seq[Long]
  )(nodeView:     ReadableNodeView): Option[Seq[Block]] = {
    // TODO: Jing - add function in nodeView.history to get multiple modifier at heights
    val blocks = blockHeights.flatMap(nodeView.history.modifierByHeight)
    if (blocks.nonEmpty) blocks.some
    else None
  }

  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: ActorSystem[_] = context.system
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }
}
