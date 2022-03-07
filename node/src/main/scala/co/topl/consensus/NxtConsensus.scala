package co.topl.consensus

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.pattern.StatusReply
import akka.util.Timeout
import cats.data.EitherT
import co.topl.consensus.NxtConsensus.{State, StateUpdate}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.db.LDBVersionedStore
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.nodeView.{KeyValueStore, LDBKeyValueStore}
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.{Int128, NetworkType}
import com.google.common.primitives.Longs

import java.io.File
import scala.concurrent.{ExecutionContext, Future}

/**
 * Consensus storage that keeps totalStake, difficulty, inflation, and height both in memory and in file
 */
object NxtConsensus {

  val actorName = "consensus-state-holder"

  // constant keys for each piece of consensus state
  private class EncodedKeys(f: String => Digest32) {
    val totalStake: Array[Byte] = f("totalStake").value
    val difficulty: Array[Byte] = f("difficulty").value
    val inflation: Array[Byte] = f("inflation").value
    val height: Array[Byte] = f("height").value
  }

  private val encodedKeys = new EncodedKeys(key => blake2b256.hash(key.getBytes("UTF-8")))

  sealed abstract class ReceivableMessage

  object ReceivableMessages {
    case class ReadConsensusState(replyTo: ActorRef[State]) extends ReceivableMessage

    case class ReadConsensusState(replyTo: ActorRef[State]) extends ReceivableMessage

    case class UpdateConsensusState(blockId: ModifierId,
                                    stateUpdate: StateUpdate,
                                    replyTo: ActorRef[StatusReply[Done]]
                                   ) extends ReceivableMessage

    case class RollbackConsensusState(blockId: ModifierId, replyTo: ActorRef[StatusReply[State]])
      extends ReceivableMessage
  }

  /**
   * Global parameters used by the consensus package.
   *
   * @param totalStake the total stake in the system
   * @param difficulty the current forging difficulty
   * @param inflation  the current value of inflation
   * @param height     the height of the main chain
   */
  case class State(totalStake: Int128, difficulty: Long, inflation: Long, height: Long)

  case class StateUpdate(
                          totalStake: Option[Int128],
                          difficulty: Option[Long],
                          inflation: Option[Long],
                          height: Option[Long]
                        )

  /**
   * Initializes a consensus variable actor. It is optional to pass in a KeyValueStore for persistence or in memory
   * store for testing. If None is provided, read or generate a LDBKeyValueStore using the settings
   *
   * @param settings    app settings
   * @param networkType network type
   * @param storageOpt  optional KeyValueStore for manual initialization or testing
   */
  def apply(settings: AppSettings,
            networkType: NetworkType,
            storage: KeyValueStore
           ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit val ec: ExecutionContext = context.executionContext

      context.log.info(s"${Console.GREEN}Consensus store actor initializing${Console.RESET}")

      val defaultTotalStake = networkType match {
        case PrivateTestnet =>
          settings.forging.privateTestnet.map(sfp => sfp.numTestnetAccts * sfp.testnetBalance).getOrElse(10000000L)
        case _ => 200000000000000000L // todo: JAA - this should be with other genesis consensus parameters
      }

      // Subscribe to new appended blocks to update the difficulty and height
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter { block =>
            val params = StateUpdate(None, Some(block.modifier.difficulty), None, Some(block.modifier.height))
            ReceivableMessages.UpdateConsensusState(block.modifier.id, params, context.system.ignoreRef)
          }
        )
      )

      context.log.info(
        s"${Console.YELLOW}Consensus Storage actor transitioning to the operational state" +
          s"${Console.RESET}"
      )

      active(
        storage,
        stateFromStorage(storage, defaultTotalStake),
        NxtLeaderElection(settings)
      )
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

  private def active(storage: KeyValueStore, consensusState: State,
                     leaderElection: NxtLeaderElection): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (_, ReceivableMessages.ReadConsensusState(replyTo)) =>
        replyTo ! consensusState
        Behaviors.same

      case (_, ReceivableMessages.UpdateConsensusState(blockId, params, replyTo)) =>
        val versionId = blockId.getIdBytes
        val (totalStake, difficulty, inflation, height) = (
          params.totalStake.getOrElse(consensusState.totalStake),
          params.difficulty.getOrElse(consensusState.difficulty),
          params.inflation.getOrElse(consensusState.inflation),
          params.height.getOrElse(consensusState.height)
        )
        val updatedParams = State(totalStake, difficulty, inflation, height)
        val toUpdate: Seq[(Array[Byte], Array[Byte])] = Seq(
          encodedKeys.totalStake -> totalStake.toByteArray,
          encodedKeys.difficulty -> Longs.toByteArray(difficulty),
          encodedKeys.inflation -> Longs.toByteArray(inflation),
          encodedKeys.height -> Longs.toByteArray(height)
        )

        // Update the storage values
        storage.update(versionId, Seq(), toUpdate)

        if (storage.latestVersionId().exists(id => id sameElements versionId)) replyTo ! StatusReply.success(Done)
        else replyTo ! StatusReply.error(new Exception("Failed to update consensus variables in storage"))

        active(storage, updatedParams)

      case (_, ReceivableMessages.RollbackConsensusState(blockId, replyTo)) =>
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
            val params = State(totalStake, difficulty, inflation, height)
            replyTo ! StatusReply.success(params)
            active(storage, params)
          case _ =>
            replyTo ! StatusReply.error(new NoSuchElementException("Failed to roll back to the given version"))
            Behaviors.same
        }
    }

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

  private def stateFromStorage(storage: KeyValueStore, defaultTotalStake: Int128): State =
    State(
      totalStakeFromStorage(storage).getOrElse(defaultTotalStake),
      difficultyFromStorage(storage).getOrElse(0L),
      inflationFromStorage(storage).getOrElse(0L),
      heightFromStorage(storage).getOrElse(0L)
    )
}

trait ConsensusViewHolderInterface {

  def read: EitherT[Future, ConsensusViewHolderInterface.Failures.Read, State]

  def update(blockId: ModifierId, consensusParamsUpdate: StateUpdate
            ): EitherT[Future, ConsensusViewHolderInterface.Failures.Update, Done]
}

object ConsensusViewHolderInterface {
  object Failures {
    case class Read(reason: Throwable)

    case class Update(reason: Throwable)

    case class Rollback(reason: Throwable)
  }
}


class ActorConsensusViewHolderInterface(actorRef: ActorRef[NxtConsensus.ReceivableMessage]
                                        )(implicit
                                          system: ActorSystem[_],
                                          timeout: Timeout
                                        ) extends ConsensusViewHolderInterface {

  import akka.actor.typed.scaladsl.AskPattern._
  import system.executionContext

  override def read: EitherT[Future, ConsensusViewHolderInterface.Failures.Read, State] = {
    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(5.seconds)
    EitherT(
      actorRef
        .ask[State](NxtConsensus.ReceivableMessages.ReadConsensusState)
        .map(Right(_))
        .recover { case e => Left(ConsensusViewHolderInterface.Failures.Read(e)) }
    )
  }

  override def update(blockId: ModifierId,
                      consensusParamsUpdate: StateUpdate
                     ): EitherT[Future, ConsensusViewHolderInterface.Failures.Update, Done] =
    EitherT.liftF(
      actorRef.askWithStatus[Done](
        NxtConsensus.ReceivableMessages.UpdateConsensusState(blockId, consensusParamsUpdate, _)
      )
    )
}
