package co.topl.consensus

import co.topl.modifier.ModifierId
import co.topl.nodeView.history.db.LDBVersionedStore
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.{LocalTestnet, PrivateTestnet}
import co.topl.utils.{Int128, Logging, NetworkType}
import com.google.common.primitives.Longs
import scorex.crypto.hash.Blake2b256

import java.io.File

/**
 * Persists parameters(totalStake, difficulty, inflation, and height) used in the consensus package.
 * @param storage the LSM store to persist values in
 * @param defaultTotalStake should be 10000000 for private and local testnet, and 200000000000000000L otherwise
 */
class ConsensusStorage(storage: Option[LDBVersionedStore], private val defaultTotalStake: Int128) extends Logging {

  // constant keys for each piece of consensus state
  private val totalStakeKey = Blake2b256("totalStake".getBytes)
  private val difficultyKey = Blake2b256("difficulty".getBytes)
  private val inflationKey = Blake2b256("inflation".getBytes)
  private val heightKey = Blake2b256("height".getBytes)

  private val defaultDifficulty: Long = 0
  private val defaultInflation: Long = 0
  private val defaultHeight: Long = 0

  private val totalStakeFromStorageOrDefault =
    storage
      .flatMap(_.get(totalStakeKey).map(v => Int128(v)))
      .getOrElse(defaultTotalStake)

  private val difficultyFromStorageOrDefault =
    storage
      .flatMap(_.get(difficultyKey).map(v => Longs.fromByteArray(v)))
      .getOrElse(defaultDifficulty)

  private val inflationFromStorageOrDefault =
    storage
      .flatMap(_.get(inflationKey).map(v => Longs.fromByteArray(v)))
      .getOrElse(defaultInflation)

  private val heightFromStorageOrDefault =
    storage
      .flatMap(_.get(heightKey).map(v => Longs.fromByteArray(v)))
      .getOrElse(defaultHeight)

  // cached state
  private var _totalStake: Int128 = totalStakeFromStorageOrDefault
  private var _difficulty: Long = difficultyFromStorageOrDefault
  private var _inflation: Long = inflationFromStorageOrDefault
  private var _height: Long = heightFromStorageOrDefault

  def totalStake: Int128 = _totalStake
  def difficulty: Long = _difficulty
  def inflation: Long = _inflation
  def height: Long = _height

  /**
   * Updates the consensus storage with the given values at a given block as the version.
   * @param blockId the block ID associated with the consensus parameters
   * @param params the current state of the parameters
   */
  def updateConsensusStorage(blockId: ModifierId, params: ConsensusParams): Unit = {
    _totalStake = params.totalStake
    _difficulty = params.difficulty
    _inflation = params.inflation
    _height = params.height

    val versionId = blockId.getIdBytes

    val totalStakePair = Seq(totalStakeKey -> params.totalStake.toByteArray)
    val difficultyPair = Seq(difficultyKey -> Longs.toByteArray(params.difficulty))
    val inflationPair = Seq(inflationKey -> Longs.toByteArray(params.inflation))
    val heightPair = Seq(heightKey -> Longs.toByteArray(params.height))

    val toUpdate = totalStakePair ++ difficultyPair ++ inflationPair ++ heightPair

    // update cached values here
    storage match {
      case Some(store) => store.update(versionId, Seq(), toUpdate)
      case _           => log.warn("Failed saving consensus params in storage")
    }
  }

  /**
   * Rolls back the current state of storage to the data within the given version.
   * @param blockId the ID of the block to rollback to
   * @return a NoStorageError if no storage exists or a Unit if successful
   */
  def rollbackTo(blockId: ModifierId): Either[NoStorageError, Unit] =
    storage match {
      case Some(store) =>
        store.rollbackTo(blockId.getIdBytes)

        // reset cached values to stored values or defaults
        _difficulty = difficultyFromStorageOrDefault
        _height = heightFromStorageOrDefault
        _inflation = inflationFromStorageOrDefault
        _totalStake = totalStakeFromStorageOrDefault

        Right(())
      case None =>
        Left(NoStorageError())
    }
}

object ConsensusStorage {

  def apply(settings: AppSettings, networkType: NetworkType): ConsensusStorage = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val defaultTotalStake = networkType match {
      case PrivateTestnet | LocalTestnet =>
        settings.forging.privateTestnet.map(sfp => sfp.numTestnetAccts * sfp.testnetBalance).getOrElse(10000000L)
      case _ => 200000000000000000L // todo: JAA - this should be with other genesis consensus parameters
    }
    val file = new File(s"$dataDir/consensus")
    file.mkdirs()

    val versionedStore = new LDBVersionedStore(file, 100)

    // todo: JAA - we need to find a better pattern than this. I see why it is the most straightforward for now,
    //       but maybe we elevate the ConsensusStorage interface up to a sealed abstract class and have two instances?
    //
    val consensusStorage = new ConsensusStorage(Some(versionedStore), defaultTotalStake)

    consensusStorage
  }

  /**
   * Initializes an empty ConsensusStorage object with no persisted store.
   * Values will be cached in memory.
   * @return the initialized storage
   */
  def emptyStorage(): ConsensusStorage = new ConsensusStorage(None, 0)

}

/**
 * Global parameters used by the consensus package.
 * @param totalStake the total stake in the system
 * @param difficulty the current forging difficulty
 * @param inflation the current value of inflation
 * @param height the height of the main chain
 */
case class ConsensusParams(totalStake: Int128, difficulty: Long, inflation: Long, height: Long)

/** Indicates that there is no persisted store available. */
case class NoStorageError()
