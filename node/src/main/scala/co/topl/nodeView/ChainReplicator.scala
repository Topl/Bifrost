package co.topl.nodeView

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.AppSettings
import co.topl.tools.exporter.{DataType, MongoExport}
import io.circe.syntax.EncoderOps
import org.mongodb.scala.result
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object ChainReplicator {

  val actorName = "ChainReplicator"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class ExportBlocks(blocks: Seq[Block]) extends ReceivableMessage

    private[nodeView] case class AppendSuccess(result: String) extends ReceivableMessage

    private[nodeView] case class AppendFailure(result: String) extends ReceivableMessage

    private[nodeView] case object Rollback extends ReceivableMessage

    private[nodeView] case object CheckDatabaseComplete extends ReceivableMessage

    private[nodeView] case class StartReplicating(replyTo: ActorRef[Done]) extends ReceivableMessage

    private[nodeView] case class StopReplicating(replyTo: ActorRef[Done]) extends ReceivableMessage

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

//      context.pipeToSelf(mongo.checkDatabase())(
//        _.fold(ReceivableMessages.Terminate, result => ReceivableMessages.CheckDatabaseComplete)
//      )

      context.pipeToSelf(mongo.checkDatabase()) {
        case Success(result) => context.log.info(s"${Console.GREEN}Collections: $result${Console.RESET}"); ReceivableMessages.CheckDatabaseComplete
        case Failure(e) => ReceivableMessages.Terminate(new Exception("Failed to query the database"))
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
      case ReceivableMessages.StartReplicating(replyTo) =>
        replyTo.tell(Done)
        uninitialized(replicateWhenReady = true)
      case ReceivableMessages.StopReplicating(replyTo) =>
        replyTo.tell(Done)
        uninitialized(replicateWhenReady = false)
      case ReceivableMessages.CheckDatabaseComplete =>
        log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
        if (replicateWhenReady) {
          context.self.tell(ReceivableMessages.StartReplicating(context.system.ignoreRef))
          active
        } else {
          paused
        }
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  def paused: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartReplicating(replyTo) =>
        replyTo.tell(Done)
        active
      case ReceivableMessages.StopReplicating(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  def active: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartReplicating(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.StopReplicating(replyTo) =>
        replyTo.tell(Done)
        paused
      case ReceivableMessages.ExportBlocks(blocks) =>
        context.pipeToSelf(mongo.insert(blocks.map(_.asJson(blockEncoder).toString))) {
          case Success(result) => ReceivableMessages.AppendSuccess(result.toString)
          case Failure(e) => ReceivableMessages.AppendFailure(e.toString)
        }
        Behaviors.same
      case ReceivableMessages.AppendSuccess(result) =>
        log.info(s"${Console.GREEN}$result${Console.RESET}")
        Behaviors.same
      case ReceivableMessages.AppendFailure(failure) =>
        log.info(s"${Console.RED}$failure${Console.RESET}")
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }
}
