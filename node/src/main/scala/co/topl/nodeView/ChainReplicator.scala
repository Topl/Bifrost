package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.ChainReplicatorSettings
import co.topl.tools.exporter.{DataType, MongoExport}
import io.circe.syntax.EncoderOps
import org.slf4j.Logger

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object ChainReplicator {

  val actorName = "ChainReplicator"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class GotNewBlock(block: Block) extends ReceivableMessage

    private[nodeView] case class ExportBlocks(blocks: Seq[Block]) extends ReceivableMessage

    private[nodeView] case class InsertionSuccess(result: Int) extends ReceivableMessage

    private[nodeView] case class InsertionFailure(error: String) extends ReceivableMessage

    private[nodeView] case class CheckMissingBlocks(maxHeight: Long) extends ReceivableMessage

    private[nodeView] case class CheckHeightsSuccess(result: Seq[Long]) extends ReceivableMessage

    private[nodeView] case class CheckHeightsFailure(error: String) extends ReceivableMessage

    private[nodeView] case class getBlocksNodeViewFailure(error: String) extends ReceivableMessage

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

      Behaviors.withTimers(timers =>
        new ChainReplicator(mongo, nodeViewHodlerRef, settings)
          .uninitialized(settings.enableChainReplicator, timers)
      )
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

  def uninitialized(
    replicateWhenReady: Boolean,
    timers:             TimerScheduler[ReceivableMessage]
  ): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.CheckDatabaseComplete =>
        if (replicateWhenReady) {
          log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
          active(0, timers)
        } else {
          Behaviors.stopped { () =>
            log.info(
              s"${Console.GREEN}Not initializing chain replicator since enableChainReplicator " +
              s"is turned off${Console.RESET}"
            )
          }
        }
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  private def active(blockCheckStart: Long, timers: TimerScheduler[ReceivableMessage]): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.GotNewBlock(block) =>
        log.info(s"${Console.GREEN}Got new block at height: ${block.height}${Console.RESET}")
        context.self ! ReceivableMessages.ExportBlocks(Seq(block))
        if (settings.checkMissingBlock)
          timers.startSingleTimer(ReceivableMessages.CheckMissingBlocks(block.height), 1.seconds)
        Behaviors.same

      case ReceivableMessages.ExportBlocks(blocks) =>
        if (blocks.nonEmpty) {
          val blocksString = blocks.map(_.asJson(blockEncoder).toString)
          context.pipeToSelf(mongo.insert(blocksString)) {
            case Success(result) => ReceivableMessages.InsertionSuccess(result.getInsertedIds.size)
            case Failure(e)      => ReceivableMessages.InsertionFailure(e.toString)
          }
        }
        Behaviors.same

      case ReceivableMessages.InsertionSuccess(result) =>
        // cleanup the log info
        log.info(s"${Console.GREEN}Successfully inserted $result blocks into AppView${Console.RESET}")
        Behaviors.same

      case ReceivableMessages.InsertionFailure(failure) =>
        log.error(s"${Console.RED}$failure${Console.RESET}")
        Behaviors.same

      case ReceivableMessages.CheckMissingBlocks(maxHeight) =>
        val blockCheckEnd: Long = blockCheckStart + settings.numberOfBlocksToCheck
        if (maxHeight >= blockCheckEnd) {
          val existingHeightsFuture =
            mongo.getExistingHeights(blockCheckStart, blockCheckEnd)
          context.pipeToSelf(existingHeightsFuture) {
            case Success(result) => ReceivableMessages.CheckHeightsSuccess(result)
            case Failure(e)      => ReceivableMessages.CheckHeightsFailure(e.toString)
          }
        }
        Behaviors.same

      case ReceivableMessages.CheckHeightsSuccess(existingHeights) =>
        val exisitingHeightsSet = existingHeights.toSet
        val blockCheckEnd: Long = blockCheckStart + settings.numberOfBlocksToCheck
        val missingBlockHeights = (blockCheckStart to blockCheckEnd).filterNot(exisitingHeightsSet.contains)
        if (missingBlockHeights.nonEmpty) {
          val getBlockAtHeightFuture = exportBlocksAtHeight(missingBlockHeights)(_)
          context.pipeToSelf(withNodeView(getBlockAtHeightFuture)) {
            case Success(exportBlocks) => exportBlocks
            case Failure(e)            => ReceivableMessages.getBlocksNodeViewFailure(e.toString)
          }
        }
        active(blockCheckEnd + 1, timers)

      case ReceivableMessages.getBlocksNodeViewFailure(failure) =>
        log.error(s"${Console.RED}$failure${Console.RESET}")
        Behaviors.same

      case ReceivableMessages.CheckHeightsFailure(failure) =>
        log.error(s"${Console.RED}$failure${Console.RESET}")
        Behaviors.same

      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  private def exportBlocksAtHeight(
    blockHeights: Seq[Long]
  )(nodeView:     ReadableNodeView): ReceivableMessages.ExportBlocks = {
    val blocks = blockHeights.flatMap(nodeView.history.modifierByHeight)
    log.info(s"${Console.GREEN}Inserting blocks at height: $blockHeights${Console.RESET}")
    ReceivableMessages.ExportBlocks(blocks)
  }

  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: ActorSystem[_] = context.system
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }
}
