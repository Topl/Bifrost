package co.topl.consensus

import co.topl.modifier.ModifierId
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.{Int128, Logging, NetworkType}
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.hash.Blake2b256

import java.io.File

class ConsensusStorage(storage: Option[LSMStore], private val defaultTotalStake: BigInt) extends Logging {

  // constant keys for each piece of consensus state
  private val totalStakeKey = ByteArrayWrapper(Blake2b256("totalStake".getBytes))
  private val difficultyKey = ByteArrayWrapper(Blake2b256("difficulty".getBytes))
  private val inflationKey = ByteArrayWrapper(Blake2b256("inflation".getBytes))
  private val heightKey = ByteArrayWrapper(Blake2b256("height".getBytes))

  private val defaultDifficulty: Long = 0
  private val defaultInflation: Long = 0
  private val defaultHeight: Long = 0

  private var _totalStake: Int128 = storage match {
    case Some(store) => store.get(totalStakeKey).map(v => Int128(v.data)).getOrElse(defaultTotalStake)
    case None => defaultTotalStake
  }

  private var _difficulty: Long = storage match {
    case Some(store) => store.get(difficultyKey).map(v => Longs.fromByteArray(v.data)).getOrElse(defaultDifficulty)
    case None => defaultDifficulty
  }

  private var _inflation: Long = storage match {
    case Some(store) => store.get(inflationKey).map(v => Longs.fromByteArray(v.data)).getOrElse(defaultInflation)
    case None => defaultInflation
  }

  private var _height: Long = storage match {
    case Some(store) => store.get(heightKey).map(v => Longs.fromByteArray(v.data)).getOrElse(defaultHeight)
    case None => defaultHeight
  }

  def totalStake: Int128 = _totalStake
  def difficulty: Long = _difficulty
  def inflation: Long = _inflation
  def height: Long = _height

  /** Updates the consensus storage with the given values at a given block as the version.
    * @param blockId the block ID associated with the consensus parameters
    * @param params the current state of the parameters
    */
  def updateConsensusStorage(blockId: ModifierId, params: ConsensusParams): Unit = {
    _totalStake = params.totalStake
    _difficulty = params.difficulty
    _inflation = params.inflation
    _height = params.height

    val versionId = toVersionId(blockId)

    val totalStakePair = Seq(totalStakeKey -> params.totalStake.toByteArray)
    val difficultyPair = Seq(difficultyKey -> Longs.toByteArray(params.difficulty))
    val inflationPair = Seq(inflationKey -> Longs.toByteArray(params.inflation))
    val heightPair = Seq(heightKey -> Longs.toByteArray(params.height))

    val toUpdate = (totalStakePair ++ difficultyPair ++ inflationPair ++ heightPair).map{
      case (k, v) => k -> ByteArrayWrapper(v)
    }

    // update cached values here
    storage match {
      case Some(store) => store.update(versionId, Seq(), toUpdate)
      case _ => log.warn("Failed saving consensus params in storage")
    }
  }

  /** Rolls back the current state of storage to the data within the given version.
    * @param blockId the ID of the block to rollback to
    * @return a NoStorageError if no storage exists or a Unit if successful
    */
  def rollbackTo(blockId: ModifierId): Either[NoStorageError, Unit] = {
    storage match {
      case Some(store) =>
        Right(store.rollback(toVersionId(blockId)))
      case None =>
        Left(NoStorageError())
    }
  }

  /** Converts the given block ID to a version ID to use with LSM Store.
    * @param blockId the ID of the block
    * @return the version ID
    */
  private def toVersionId(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(blockId.bytes)

}

object ConsensusStorage {

  def apply(settings: AppSettings, networkType: NetworkType): ConsensusStorage = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val defaultTotalStake = networkType match {
      case PrivateTestnet =>
        settings.forging.privateTestnet.map (sfp => sfp.numTestnetAccts * sfp.testnetBalance).getOrElse(10000000L)
      case _ => 200000000000000000L

    }
    val file = new File(s"$dataDir/consensusStorage")
    file.mkdirs()
    val storage = new LSMStore(file)
    val consensusStorage = new ConsensusStorage(Some(storage), defaultTotalStake)

    consensusStorage
  }

  def emptyStorage(): ConsensusStorage = new ConsensusStorage(None, 0)

}

case class ConsensusParams(totalStake: Int128, difficulty: Long, inflation: Long, height: Long)

case class NoStorageError()
