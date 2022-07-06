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
import co.topl.consensus.NxtConsensus.StateUpdate
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
import scala.util.Try

/**
 * Consensus storage that keeps totalStake, difficulty, inflation, and height both in memory and in file
 */
// todo: I am calling this NxtConsensus because the variables here are specific to NxtConsensus state but it
// seems like we could generalize this further by abstracting the type of state that the ConsansusStateHolder manages
object NxtConsensus {

  final val actorName = "consensus-view-holder"
  final val serviceKey: ServiceKey[ReceivableMessage] = ServiceKey(actorName)

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    case class LookupState(replyTo: ActorRef[StatusReply[NxtConsensus.State]]) extends ReceivableMessage

    case class UpdateState(blockId: ModifierId, stateUpdate: StateUpdate, replyTo: ActorRef[StatusReply[Done]])
        extends ReceivableMessage

    case class RollbackState(blockId: ModifierId, replyTo: ActorRef[StatusReply[State]]) extends ReceivableMessage
  }

  case class State(totalStake: Int128, inflation: Long)

  object State {
    val empty: NxtConsensus.State = State(Int128(0L), 0L)
  }

  case class StateUpdate(
    totalStake: Option[Int128],
    inflation:  Option[Long]
  )

  case class Genesis(block: Block, state: State)

  /**
   * Initializes a consensus variable actor. It is optional to pass in a KeyValueStore for persistence or in memory
   * store for testing. If None is provided, read or generate a LDBKeyValueStore using the settings
   *
   * @param settings    app settings
   * @param networkType network type
   * @param storageOpt  optional KeyValueStore for manual initialization or testing
   */
  def apply(
    settings: AppSettings,
    storage:  KeyValueStore
  ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit val ec: ExecutionContext = context.executionContext

      context.log.info(s"${Console.GREEN}Consensus store actor initializing${Console.RESET}")

      // Subscribe to new appended blocks to update the difficulty and height
      // todo: JAA - before implementing anything to do with updating inflation or the totalStake amount, we
      //  first need to figure out a way to retrieve data by block id (like the event source tree in tetra)
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter { block =>
            ReceivableMessages.UpdateState(block.modifier.id, StateUpdate(None, None), context.system.ignoreRef)
          }
        )
      )

      context.log.info(
        s"${Console.YELLOW}Consensus Storage actor transitioning to the operational state${Console.RESET}"
      )

      active(storage)
    }

  /**
   * Read or generate LDB key value store for persistence
   *
   * @param settings for getting the data directory
   * @return LDBKeyValueStore for the consensus variable actor
   */
  def readOrGenerateConsensusStore(settings: AppSettings): KeyValueStore = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val file = new File(s"$dataDir/consensus")
    file.mkdirs()
    new LDBKeyValueStore(new LDBVersionedStore(file, settings.application.consensusStoreVersionsToKeep))
  }

  private def active(storage: KeyValueStore): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (_, ReceivableMessages.LookupState(replyTo)) =>
        replyTo.tell(
          stateFromStorage(storage).fold[StatusReply[State]](e => StatusReply.error(e.toString), StatusReply.success)
        )
        Behaviors.same

      case (_, ReceivableMessages.UpdateState(blockId, params, replyTo)) =>
        val versionId = blockId.getIdBytes
        val (totalStake, inflation) = (
          params.totalStake.getOrElse(totalStakeFromStorage(storage).getOrElse(State.empty.totalStake)),
          params.inflation.getOrElse(inflationFromStorage(storage).getOrElse(State.empty.inflation))
        )
        val toUpdate: Seq[(Array[Byte], Array[Byte])] = Seq(
          encodedKeys.totalStake -> totalStake.toByteArray,
          encodedKeys.inflation  -> Longs.toByteArray(inflation)
        )

        // Update the storage values
        storage.update(versionId, Seq(), toUpdate)

        if (storage.latestVersionId().exists(id => id sameElements versionId)) replyTo ! StatusReply.success(Done)
        else replyTo ! StatusReply.error(new Exception("Failed to update consensus variables in storage"))

        active(storage)

      case (_, ReceivableMessages.RollbackState(blockId, replyTo)) =>
        storage.rollbackTo(blockId.getIdBytes)
        // Check if the storage is rolled back to the given version by comparing the last version in storage
        val rollBackResult = storage.latestVersionId().exists(id => id sameElements blockId.getIdBytes)
        (
          totalStakeFromStorage(storage),
          inflationFromStorage(storage)
        ) match {
          case (Right(totalStake), Right(inflation)) if rollBackResult =>
            val updatedState = State(totalStake, inflation)
            replyTo ! StatusReply.success(updatedState)
            active(storage)
          case _ =>
            replyTo ! StatusReply.error(new NoSuchElementException("Failed to roll back to the given version"))
            Behaviors.same
        }
    }

  // constant keys for each piece of consensus state
  private class EncodedKeys(f: String => Digest32) {
    val totalStake: Array[Byte] = f("totalStake").value
    val inflation: Array[Byte] = f("inflation").value
  }

  private val encodedKeys = new EncodedKeys(key => blake2b256.hash(key.getBytes("UTF-8")))

  private def totalStakeFromStorage(
    storage: KeyValueStore
  ): Either[ConsensusInterface.ReadStateFailures.FailedToReadTotalStake.type, Int128] =
    storage.get(encodedKeys.totalStake) match {
      case Some(value) => Right(Int128(value))
      case None        => Left(ConsensusInterface.ReadStateFailures.FailedToReadTotalStake)
    }

  private def inflationFromStorage(
    storage: KeyValueStore
  ): Either[ConsensusInterface.ReadStateFailures.FailedToReadInflation.type, Long] =
    storage.get(encodedKeys.inflation) match {
      case Some(value) => Right(Longs.fromByteArray(value))
      case None        => Left(ConsensusInterface.ReadStateFailures.FailedToReadInflation)
    }

  private def stateFromStorage(
    storage: KeyValueStore
  ): Either[ConsensusInterface.ReadStateFailure, NxtConsensus.State] = for {
    totalStake <- totalStakeFromStorage(storage)
    inflation  <- inflationFromStorage(storage)
  } yield State(totalStake, inflation)
}

trait ConsensusReader {
  def lookupState: EitherT[Future, ConsensusInterface.ReadStateFailure, NxtConsensus.State]
}

trait ConsensusInterface extends ConsensusReader {
  def update(blockId: ModifierId, stateUpdate: StateUpdate): EitherT[Future, ConsensusInterface.UpdateFailure, Done]
}

object ConsensusInterface {

  sealed trait ReadStateFailure

  object ReadStateFailures {
    case class InternalException(reason: Throwable) extends ReadStateFailure
    case object FailedToReadTotalStake extends ReadStateFailure
    case object FailedToReadInflation extends ReadStateFailure
  }

  sealed trait UpdateFailure
  sealed trait RollbackFailure
}

class ActorConsensusInterface(actorRef: ActorRef[NxtConsensus.ReceivableMessage])(implicit
  system:                               ActorSystem[_],
  timeout:                              Timeout
) extends ConsensusInterface {

  import akka.actor.typed.scaladsl.AskPattern._
  import system.executionContext

  override def lookupState: EitherT[Future, ConsensusInterface.ReadStateFailure, NxtConsensus.State] =
    EitherT.liftF(actorRef.askWithStatus(x => NxtConsensus.ReceivableMessages.LookupState(x)))

  override def update(
    blockId:     ModifierId,
    stateUpdate: StateUpdate
  ): EitherT[Future, ConsensusInterface.UpdateFailure, Done] =
    EitherT.liftF(actorRef.askWithStatus(NxtConsensus.ReceivableMessages.UpdateState(blockId, stateUpdate, _)))
}
