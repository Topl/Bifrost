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
import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.{Int128, NetworkType}
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
    case class ReadState(replyTo: ActorRef[State]) extends ReceivableMessage

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

  case class StateUpdate(
    totalStake: Option[Int128],
    difficulty: Option[Long],
    inflation:  Option[Long],
    height:     Option[Long]
  )

  case class View(state: State, leaderElection: NxtLeaderElection, protocolVersions: ProtocolVersioner)

  /**
   * Initializes a consensus variable actor. It is optional to pass in a KeyValueStore for persistence or in memory
   * store for testing. If None is provided, read or generate a LDBKeyValueStore using the settings
   *
   * @param settings    app settings
   * @param networkType network type
   * @param storageOpt  optional KeyValueStore for manual initialization or testing
   */
  def apply(
    settings:    AppSettings,
    networkType: NetworkType,
    storage:     KeyValueStore
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
            ReceivableMessages.UpdateState(block.modifier.id, params, context.system.ignoreRef)
          }
        )
      )

      context.log.info(
        s"${Console.YELLOW}Consensus Storage actor transitioning to the operational state" +
        s"${Console.RESET}"
      )

      val protocolVersioner = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)

      val leaderElection = new NxtLeaderElection(protocolVersioner)

      active(storage, View(stateFromStorage(storage, defaultTotalStake), leaderElection, protocolVersioner))
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
        replyTo ! consensusView.state
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

  private def stateFromStorage(storage: KeyValueStore, defaultTotalStake: Int128): State =
    State(
      totalStakeFromStorage(storage).getOrElse(defaultTotalStake),
      difficultyFromStorage(storage).getOrElse(0L),
      inflationFromStorage(storage).getOrElse(0L),
      heightFromStorage(storage).getOrElse(0L)
    )
}

trait ConsensusReader {
  def withView[T](f: NxtConsensus.View => T): EitherT[Future, ConsensusInterface.Failures.Read, T]
}

trait ConsensusInterface extends ConsensusReader {
  def update(blockId: ModifierId, stateUpdate: StateUpdate): EitherT[Future, ConsensusInterface.Failures.Update, Done]
}

object ConsensusInterface {

  object Failures {
    case class Read(reason: Throwable)

    case class Update(reason: Throwable)

    case class Rollback(reason: Throwable)
  }
}

class ActorConsensusInterface(actorRef: ActorRef[NxtConsensus.ReceivableMessage])(implicit
  system:                               ActorSystem[_],
  timeout:                              Timeout
) extends ConsensusInterface {

  import akka.actor.typed.scaladsl.AskPattern._
  import system.executionContext

  override def withView[T](f: NxtConsensus.View => T): EitherT[Future, ConsensusInterface.Failures.Read, T] = {
    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(5.seconds)
    EitherT(
      actorRef
        .askWithStatus[T](NxtConsensus.ReceivableMessages.WithView(f, _))
        .map(Right(_))
        .recover { case e => Left(ConsensusInterface.Failures.Read(e)) }
    )
  }

  override def update(
    blockId:     ModifierId,
    stateUpdate: StateUpdate
  ): EitherT[Future, ConsensusInterface.Failures.Update, Done] =
    EitherT.liftF(
      actorRef.askWithStatus[Done](
        NxtConsensus.ReceivableMessages.UpdateState(blockId, stateUpdate, _)
      )
    )
}
