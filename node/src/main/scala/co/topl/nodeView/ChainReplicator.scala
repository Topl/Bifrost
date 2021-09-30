package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.AppSettings
import co.topl.tools.exporter.{DataType, MongoExport}
import io.circe.syntax.EncoderOps
import org.slf4j.Logger

import scala.util.{Failure, Success}

object ChainReplicator {

  val actorName = "ChainReplicator"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class ExportBlocks(blocks: Seq[Block]) extends ReceivableMessage

    private[nodeView] case class InsertionSuccess(result: String) extends ReceivableMessage

    private[nodeView] case class InsertionFailure(result: String) extends ReceivableMessage

    private[nodeView] case object Rollback extends ReceivableMessage

    private[nodeView] case object CheckDatabaseComplete extends ReceivableMessage

    private[nodeView] case class Terminate(reason: Throwable) extends ReceivableMessage
  }

  def apply(
    nodeViewHodlerRef: ActorRef[NodeViewHolder.ReceivableMessage],
    settings:          AppSettings
  ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter(block => ReceivableMessages.ExportBlocks(Seq(block.modifier)))
        )
      )
      context.log.info(s"${Console.GREEN}Chain replicator initializing${Console.RESET}")

      val mongo =
        MongoExport(
          settings.chainReplicator.uri.getOrElse("mongodb://localhost"),
          settings.chainReplicator.database.getOrElse("bifrost"),
          settings.chainReplicator.collection.getOrElse("blocks"),
          DataType.Block
        )

      context.pipeToSelf(mongo.checkValidConnection()) {
        case Success(result) =>
          context.log.info(s"${Console.GREEN}Found collections in database: $result${Console.RESET}")
          ReceivableMessages.CheckDatabaseComplete
        case Failure(e) => ReceivableMessages.Terminate(new Exception(s"Failed to query the database: $e"))
      }

      new ChainReplicator(mongo, nodeViewHodlerRef).uninitialized(settings.chainReplicator.enableChainReplicator)
    }
}

private class ChainReplicator(
  mongo:             MongoExport,
  nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage]
)(implicit
  context: ActorContext[ChainReplicator.ReceivableMessage]
) {
  implicit private val log: Logger = context.log

  import ChainReplicator._
  import co.topl.tool.Exporter.blockEncoder

  def uninitialized(replicateWhenReady: Boolean): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.CheckDatabaseComplete =>
        if (replicateWhenReady) {
          log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
          active
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

  def active: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.ExportBlocks(blocks) =>
        val blocksString = blocks.map(_.asJson(blockEncoder).toString)
        context.pipeToSelf(mongo.insert(blocksString)) {
          case Success(result) => ReceivableMessages.InsertionSuccess(result.toString)
          case Failure(e)      => ReceivableMessages.InsertionFailure(e.toString)
        }
        Behaviors.same
      case ReceivableMessages.InsertionSuccess(result) =>
        log.info(s"${Console.GREEN}$result${Console.RESET}")
        Behaviors.same
      case ReceivableMessages.InsertionFailure(failure) =>
        log.info(s"${Console.RED}$failure${Console.RESET}")
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }
}
