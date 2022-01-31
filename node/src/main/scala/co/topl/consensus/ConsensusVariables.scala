package co.topl.consensus

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.pattern.StatusReply
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.db.LDBVersionedStore
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.LDBKeyValueStore
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.{Int128, NetworkType}
import com.google.common.primitives.Longs

import java.io.File
import scala.concurrent.ExecutionContext

/**
 * Consensus storage that keeps totalStake, difficulty, inflation, and height both in memory and in file
 */
object ConsensusVariables {

  val actorName = "ConsensusVariables"

  // constant keys for each piece of consensus state
  private def byteArrayWrappedKey(name: String): Digest32 = blake2b256.hash(name.getBytes)
  private val totalStakeKey = byteArrayWrappedKey("totalStake")
  private val difficultyKey = byteArrayWrappedKey("difficulty")
  private val inflationKey = byteArrayWrappedKey("inflation")
  private val heightKey = byteArrayWrappedKey("height")

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    case class UpdateConsensusVariables(
      blockId: ModifierId,
      params:  ConsensusParamsUpdate,
      replyTo: ActorRef[StatusReply[Done]]
    ) extends ReceivableMessage

    case class RollBackTo(blockId: ModifierId, replyTo: ActorRef[StatusReply[ConsensusParams]])
        extends ReceivableMessage

    case class GetConsensusVariables(replyTo: ActorRef[ConsensusParams]) extends ReceivableMessage

  }

  def apply(
    settings:    AppSettings,
    networkType: NetworkType
  ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit val ec: ExecutionContext = context.executionContext

      context.log.info(s"${Console.GREEN}Consensus variable actor initializing${Console.RESET}")

      // Read or generate LDB key value store for persistence
      val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
      val defaultTotalStake = networkType match {
        case PrivateTestnet =>
          settings.forging.privateTestnet.map(sfp => sfp.numTestnetAccts * sfp.testnetBalance).getOrElse(10000000L)
        case _ => 200000000000000000L // todo: JAA - this should be with other genesis consensus parameters
      }
      val file = new File(s"$dataDir/consensus")
      file.mkdirs()
      val versionedStore: LDBKeyValueStore =
        new LDBKeyValueStore(new LDBVersionedStore(file, settings.application.consensusStoreVersionsToKeep))

      // Subscribe to new appended blocks to update the difficulty and height
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter { block =>
            val params = ConsensusParamsUpdate(None, Some(block.modifier.difficulty), None, Some(block.modifier.height))
            ReceivableMessages.UpdateConsensusVariables(block.modifier.id, params, context.system.ignoreRef)
          }
        )
      )

      context.log.info(
        s"${Console.YELLOW}Consensus Storage actor transitioning to the operational state" +
        s"${Console.RESET}"
      )

      active(
        versionedStore,
        paramsFromStorage(versionedStore, defaultTotalStake)
      )
    }

  private def active(storage: LDBKeyValueStore, consensusParams: ConsensusParams): Behavior[ReceivableMessage] =
    Behaviors.receivePartial {
      case (_, ReceivableMessages.GetConsensusVariables(replyTo)) =>
        replyTo ! consensusParams
        Behaviors.same

      case (_, ReceivableMessages.UpdateConsensusVariables(blockId, params, replyTo)) =>
        val versionId = blockId.getIdBytes
        val (totalStake, difficulty, inflation, height) = (
          params.totalStake.getOrElse(consensusParams.totalStake),
          params.difficulty.getOrElse(consensusParams.difficulty),
          params.inflation.getOrElse(consensusParams.inflation),
          params.height.getOrElse(consensusParams.height)
        )
        val updatedParams = ConsensusParams(totalStake, difficulty, inflation, height)
        val totalStakePair = Seq(totalStakeKey.value -> totalStake.toByteArray)
        val difficultyPair = Seq(difficultyKey.value -> Longs.toByteArray(difficulty))
        val inflationPair = Seq(inflationKey.value -> Longs.toByteArray(inflation))
        val heightPair = Seq(heightKey.value -> Longs.toByteArray(height))
        val toUpdate = totalStakePair ++ difficultyPair ++ inflationPair ++ heightPair

        // Update the storage values
        storage.update(versionId, Seq(), toUpdate)

        replyTo ! StatusReply.success(Done)
        active(storage, updatedParams)

      case (_, ReceivableMessages.RollBackTo(blockId, replyTo)) =>
        storage.rollbackTo(blockId.getIdBytes)
        // Check if the storage is rolled back to the given version by comparing the last version in storage
        val rollBackResult = storage.latestVersionId().getOrElse(Array[String]()) sameElements blockId.getIdBytes
        (
          totalStakeFromStorage(storage),
          difficultyFromStorage(storage),
          inflationFromStorage(storage),
          heightFromStorage(storage)
        ) match {
          case (Some(totalStake), Some(difficulty), Some(inflation), Some(height)) if rollBackResult =>
            val params = ConsensusParams(totalStake, difficulty, inflation, height)
            replyTo ! StatusReply.success(params)
            active(storage, params)
          case _ =>
            replyTo ! StatusReply.error(new NoSuchElementException("Failed to roll back to the given version"))
            Behaviors.same
        }
    }

  /**
   * Global parameters used by the consensus package.
   *
   * @param totalStake the total stake in the system
   * @param difficulty the current forging difficulty
   * @param inflation  the current value of inflation
   * @param height     the height of the main chain
   */
  case class ConsensusParams(totalStake: Int128, difficulty: Long, inflation: Long, height: Long)

  case class ConsensusParamsUpdate(
    totalStake: Option[Int128],
    difficulty: Option[Long],
    inflation:  Option[Long],
    height:     Option[Long]
  )

  private def totalStakeFromStorage(storage: LDBKeyValueStore): Option[Int128] =
    storage
      .get(totalStakeKey.value)
      .map(Int128(_))

  private def difficultyFromStorage(storage: LDBKeyValueStore): Option[Long] =
    storage
      .get(difficultyKey.value)
      .map(Longs.fromByteArray)

  private def inflationFromStorage(storage: LDBKeyValueStore): Option[Long] =
    storage
      .get(inflationKey.value)
      .map(Longs.fromByteArray)

  private def heightFromStorage(storage: LDBKeyValueStore): Option[Long] =
    storage
      .get(heightKey.value)
      .map(Longs.fromByteArray)

  private def paramsFromStorage(storage: LDBKeyValueStore, defaultTotalStake: Int128): ConsensusParams =
    ConsensusParams(
      totalStakeFromStorage(storage).getOrElse(defaultTotalStake),
      difficultyFromStorage(storage).getOrElse(0L),
      inflationFromStorage(storage).getOrElse(0L),
      heightFromStorage(storage).getOrElse(0L)
    )
}
