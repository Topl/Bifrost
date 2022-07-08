package co.topl.consensus

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.pattern.StatusReply
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.db.LDBVersionedStore
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.nodeView.{KeyValueStore, LDBKeyValueStore}
import co.topl.settings.AppSettings
import co.topl.utils.Int128
import com.google.common.primitives.Longs

import java.io.File
import scala.concurrent.{ExecutionContext, Future}

object NxtConsensus {
  case class State(totalStake: Int128, inflation: Long)

  case class StateUpdate(
    totalStake: Option[Int128],
    inflation:  Option[Long]
  )

  case class Genesis(block: Block, state: NxtConsensus.State)
}

/**
 * Consensus storage that keeps totalStake, difficulty, inflation, and height both in memory and in file
 */
object ConsensusHolder {

  final val actorName = "consensus-view-holder"
  final val serviceKey: ServiceKey[ReceivableMessage] = ServiceKey(actorName)

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    case class LookupState(replyTo: ActorRef[StatusReply[NxtConsensus.State]]) extends ReceivableMessage

    case class UpdateState(
      blockId:     ModifierId,
      stateUpdate: NxtConsensus.StateUpdate,
      replyTo:     ActorRef[StatusReply[Done]]
    ) extends ReceivableMessage

    case class RollbackState(blockId: ModifierId, replyTo: ActorRef[StatusReply[NxtConsensus.State]])
        extends ReceivableMessage
  }

  /**
   * Initializes a consensus variable actor. It is optional to pass in a KeyValueStore for persistence or in memory
   * store for testing. If None is provided, read or generate a LDBKeyValueStore using the settings
   */
  def apply(settings: AppSettings): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit val ec: ExecutionContext = context.executionContext

      context.log.info(s"${Console.GREEN}Consensus store actor initializing${Console.RESET}")

      // Subscribe to new appended blocks to update the difficulty and height
      // todo: JAA - before implementing anything to do with updating inflation or the totalStake amount, we
      //  first need to figure out a way to retrieve data by block id (like the event source tree in tetra)
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter { block =>
            ReceivableMessages.UpdateState(
              block.modifier.id,
              NxtConsensus.StateUpdate(None, None),
              context.system.ignoreRef
            )
          }
        )
      )

      val storage = readOrGenerateConsensusStore(settings)

      stateFromStorage(storage) match {
        case Left(error) => throw new Exception(error.toString)
        case Right(state) =>
          context.log.info(
            s"${Console.YELLOW}Consensus Storage actor transitioning to the operational state${Console.RESET}"
          )
          StatusReply.success(state)
          active(storage, state)
      }
    }

  private def active(storage: KeyValueStore, currentState: NxtConsensus.State): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (_, ReceivableMessages.LookupState(replyTo)) =>
        replyTo ! StatusReply.success(currentState)
        Behaviors.same

      case (_, ReceivableMessages.UpdateState(blockId, params, replyTo)) =>
        val versionId = blockId.getIdBytes
        val (totalStake, inflation) = (
          params.totalStake.getOrElse(currentState.totalStake),
          params.inflation.getOrElse(currentState.inflation)
        )
        val toUpdate: Seq[(Array[Byte], Array[Byte])] = Seq(
          encodedKeys.totalStake -> totalStake.toByteArray,
          encodedKeys.inflation  -> Longs.toByteArray(inflation)
        )

        // Update the storage values
        storage.update(versionId, Seq(), toUpdate)

        if (storage.latestVersionId().exists(id => id sameElements versionId)) replyTo ! StatusReply.success(Done)
        else replyTo ! StatusReply.error(new Exception("Failed to update consensus variables in storage"))

        active(storage, NxtConsensus.State(totalStake, inflation))

      case (_, ReceivableMessages.RollbackState(blockId, replyTo)) =>
        storage.rollbackTo(blockId.getIdBytes)
        // Check if the storage is rolled back to the given version by comparing the last version in storage
        if (storage.latestVersionId().exists(id => id sameElements blockId.getIdBytes))
          stateFromStorage(storage) match {
            case Left(error) =>
              replyTo ! StatusReply.error(error.toString)
              Behaviors.same
            case Right(state) =>
              StatusReply.success(state)
              active(storage, state)
          }
        else {
          replyTo ! StatusReply.error(
            new Exception(
              s"Ignoring update message as Consensus storage failed to rollback to version: ${blockId.toString}"
            )
          )
          Behaviors.same
        }
    }

  /**
   * Read or generate LDB key value store for persistence
   *
   * @param settings for getting the data directory
   * @return LDBKeyValueStore for the consensus variable actor
   */
  private def readOrGenerateConsensusStore(settings: AppSettings): KeyValueStore = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val file = new File(s"$dataDir/consensus")
    file.mkdirs()
    new LDBKeyValueStore(new LDBVersionedStore(file, settings.application.consensusStoreVersionsToKeep))
  }

  // constant keys for each piece of consensus state
  private class EncodedKeys(f: String => Digest32) {
    val totalStake: Array[Byte] = f("totalStake").value
    val inflation: Array[Byte] = f("inflation").value
  }

  private val encodedKeys = new EncodedKeys(key => blake2b256.hash(key.getBytes("UTF-8")))

  private def stateFromStorage(
    storage: KeyValueStore
  ): Either[ConsensusHolderInterface.ReadStateFailure, NxtConsensus.State] = for {
    totalStake <- storage.get(encodedKeys.totalStake) match {
      case Some(value) => Right(Int128(value))
      case None        => Left(ConsensusHolderInterface.ReadStateFailures.FailedToReadTotalStake)
    }
    inflation <- storage.get(encodedKeys.inflation) match {
      case Some(value) => Right(Longs.fromByteArray(value))
      case None        => Left(ConsensusHolderInterface.ReadStateFailures.FailedToReadInflation)
    }
  } yield NxtConsensus.State(totalStake, inflation)
}

trait ConsensusReader {
  def lookupState: EitherT[Future, ConsensusHolderInterface.ReadStateFailure, NxtConsensus.State]
}

trait ConsensusHolderInterface extends ConsensusReader {

  def update(
    blockId:     ModifierId,
    stateUpdate: NxtConsensus.StateUpdate
  ): EitherT[Future, ConsensusHolderInterface.UpdateFailure, Done]
}

object ConsensusHolderInterface {

  sealed trait ReadStateFailure

  object ReadStateFailures {
    case class InternalException(reason: Throwable) extends ReadStateFailure
    case object FailedToReadTotalStake extends ReadStateFailure
    case object FailedToReadInflation extends ReadStateFailure
  }

  sealed trait UpdateFailure
  sealed trait RollbackFailure
}

class ActorConsensusHolderInterface(actorRef: ActorRef[ConsensusHolder.ReceivableMessage])(implicit
  system:                                     ActorSystem[_],
  timeout:                                    Timeout
) extends ConsensusHolderInterface {

  import akka.actor.typed.scaladsl.AskPattern._
  import system.executionContext

  override def lookupState: EitherT[Future, ConsensusHolderInterface.ReadStateFailure, NxtConsensus.State] =
    EitherT.liftF(actorRef.askWithStatus(x => ConsensusHolder.ReceivableMessages.LookupState(x)))

  override def update(
    blockId:     ModifierId,
    stateUpdate: NxtConsensus.StateUpdate
  ): EitherT[Future, ConsensusHolderInterface.UpdateFailure, Done] =
    EitherT.liftF(actorRef.askWithStatus(ConsensusHolder.ReceivableMessages.UpdateState(blockId, stateUpdate, _)))
}
