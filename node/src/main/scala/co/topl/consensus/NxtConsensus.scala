package co.topl.consensus

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.pattern.StatusReply
import akka.util.Timeout
import cats.data.EitherT
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
    case class ReadState(replyTo: ActorRef[StatusReply[State]]) extends ReceivableMessage

    case class WithView[T](f: NxtConsensus.View => T, replyTo: ActorRef[StatusReply[T]]) extends ReceivableMessage {

      private[consensus] def run(consensusView: NxtConsensus.View): Unit =
        replyTo.tell(
          Try(f(consensusView)).fold[StatusReply[T]](e => StatusReply.error(e), StatusReply.success)
        )
    }

    case class UpdateState(blockId: ModifierId, stateUpdate: StateUpdate, replyTo: ActorRef[StatusReply[Done]])
        extends ReceivableMessage

    case class RollbackState(blockId: ModifierId, replyTo: ActorRef[StatusReply[State]]) extends ReceivableMessage
  }

  case class State(totalStake: Int128, difficulty: Long, inflation: Long, height: Long)

  object State {
    val empty: NxtConsensus.State = State(Int128(0L), 0L, 0L, 0L)
  }

  case class StateUpdate(
    totalStake: Option[Int128],
    difficulty: Option[Long],
    inflation:  Option[Long],
    height:     Option[Long]
  )

  case class View(
    state:          State,
    leaderElection: NxtLeaderElection,
    validators:     NxtConsensus.State => Seq[BlockValidator[_]]
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
    settings:                   AppSettings,
    storage:                    KeyValueStore
  )(implicit protocolVersioner: ProtocolVersioner): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit val ec: ExecutionContext = context.executionContext

      context.log.info(s"${Console.GREEN}Consensus store actor initializing${Console.RESET}")

      // Subscribe to new appended blocks to update the difficulty and height
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter { block =>
            val params = StateUpdate(None, Some(block.modifier.difficulty), None, Some(block.modifier.height))
            ReceivableMessages.UpdateState(block.modifier.id, params, context.system.ignoreRef)
          }
        )
      )

      context.log.info(
        s"${Console.YELLOW}Consensus Storage actor transitioning to the operational state${Console.RESET}"
      )

      val leaderElection = new NxtLeaderElection(protocolVersioner)

      val validators = (consensusState: NxtConsensus.State) =>
        Seq(
          new BlockValidators.DifficultyValidator(leaderElection),
          new BlockValidators.HeightValidator,
          new BlockValidators.EligibilityValidator(leaderElection, consensusState),
          new BlockValidators.SyntaxValidator(consensusState),
          new BlockValidators.TimestampValidator
        )

      active(storage, View(stateFromStorage(storage), leaderElection, validators))
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

  private def active(storage: KeyValueStore, consensusView: View): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (_, ReceivableMessages.ReadState(replyTo)) =>
        replyTo ! StatusReply.success(consensusView.state)
        Behaviors.same

      case (_, r: ReceivableMessages.WithView[_]) =>
        r.run(consensusView)
        Behaviors.same

      case (_, ReceivableMessages.UpdateState(blockId, params, replyTo)) =>
        val versionId = blockId.getIdBytes
        val (totalStake, difficulty, inflation, height) = (
          params.totalStake.getOrElse(consensusView.state.totalStake),
          params.difficulty.getOrElse(consensusView.state.difficulty),
          params.inflation.getOrElse(consensusView.state.inflation),
          params.height.getOrElse(consensusView.state.height)
        )
        val updatedState = State(totalStake, difficulty, inflation, height)
        val toUpdate: Seq[(Array[Byte], Array[Byte])] = Seq(
          encodedKeys.totalStake -> totalStake.toByteArray,
          encodedKeys.difficulty -> Longs.toByteArray(difficulty),
          encodedKeys.inflation  -> Longs.toByteArray(inflation),
          encodedKeys.height     -> Longs.toByteArray(height)
        )

        // Update the storage values
        storage.update(versionId, Seq(), toUpdate)

        if (storage.latestVersionId().exists(id => id sameElements versionId)) replyTo ! StatusReply.success(Done)
        else replyTo ! StatusReply.error(new Exception("Failed to update consensus variables in storage"))

        active(storage, consensusView.copy(state = updatedState))

      case (_, ReceivableMessages.RollbackState(blockId, replyTo)) =>
        storage.rollbackTo(blockId.getIdBytes)
        // Check if the storage is rolled back to the given version by comparing the last version in storage
        val rollBackResult = storage.latestVersionId().exists(id => id sameElements blockId.getIdBytes)
        (
          totalStakeFromStorage(storage),
          difficultyFromStorage(storage),
          inflationFromStorage(storage),
          heightFromStorage(storage)
        ) match {
          case (Some(totalStake), Some(difficulty), Some(inflation), Some(height)) if rollBackResult =>
            val updatedState = State(totalStake, difficulty, inflation, height)
            replyTo ! StatusReply.success(updatedState)
            active(storage, consensusView.copy(state = updatedState))
          case _ =>
            replyTo ! StatusReply.error(new NoSuchElementException("Failed to roll back to the given version"))
            Behaviors.same
        }
    }

  // constant keys for each piece of consensus state
  private class EncodedKeys(f: String => Digest32) {
    val totalStake: Array[Byte] = f("totalStake").value
    val difficulty: Array[Byte] = f("difficulty").value
    val inflation: Array[Byte] = f("inflation").value
    val height: Array[Byte] = f("height").value
  }

  private val encodedKeys = new EncodedKeys(key => blake2b256.hash(key.getBytes("UTF-8")))

  private def totalStakeFromStorage(storage: KeyValueStore): Option[Int128] =
    storage
      .get(encodedKeys.totalStake)
      .map(Int128(_))

  private def difficultyFromStorage(storage: KeyValueStore): Option[Long] =
    storage
      .get(encodedKeys.difficulty)
      .map(Longs.fromByteArray)

  private def inflationFromStorage(storage: KeyValueStore): Option[Long] =
    storage
      .get(encodedKeys.inflation)
      .map(Longs.fromByteArray)

  private def heightFromStorage(storage: KeyValueStore): Option[Long] =
    storage
      .get(encodedKeys.height)
      .map(Longs.fromByteArray)

  private def stateFromStorage(storage: KeyValueStore): State =
    State(
      totalStakeFromStorage(storage).getOrElse(0L),
      difficultyFromStorage(storage).getOrElse(0L),
      inflationFromStorage(storage).getOrElse(0L),
      heightFromStorage(storage).getOrElse(0L)
    )
}

trait ConsensusReader {
  def readState: EitherT[Future, ConsensusInterface.ReadStateFailure, NxtConsensus.State]
  def withView[T](f: NxtConsensus.View => T): EitherT[Future, ConsensusInterface.WithViewFailure, T]
}

trait ConsensusInterface extends ConsensusReader {
  def update(blockId: ModifierId, stateUpdate: StateUpdate): EitherT[Future, ConsensusInterface.UpdateFailure, Done]
}

object ConsensusInterface {

  sealed trait ReadStateFailure
  sealed trait WithViewFailure

  object WithViewFailures {
    case class InternalException(reason: Throwable) extends WithViewFailure
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

  override def readState: EitherT[Future, ConsensusInterface.ReadStateFailure, NxtConsensus.State] =
    EitherT.liftF(
      actorRef.askWithStatus[NxtConsensus.State](NxtConsensus.ReceivableMessages.ReadState)
    )

  override def withView[T](f: NxtConsensus.View => T): EitherT[Future, ConsensusInterface.WithViewFailure, T] = {
    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(5.seconds)
    EitherT(
      actorRef
        .askWithStatus[T](NxtConsensus.ReceivableMessages.WithView(f, _))
        .map(Right(_))
        .recover { case e => Left(ConsensusInterface.WithViewFailures.InternalException(e)) }
    )
  }

  override def update(
    blockId:     ModifierId,
    stateUpdate: StateUpdate
  ): EitherT[Future, ConsensusInterface.UpdateFailure, Done] =
    EitherT.liftF(
      actorRef.askWithStatus[Done](
        NxtConsensus.ReceivableMessages.UpdateState(blockId, stateUpdate, _)
      )
    )
}
