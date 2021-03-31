package co.topl.consensus

import java.io.File
import co.topl.modifier.ModifierId
import co.topl.settings.AppSettings
import co.topl.utils.{Int128, Logging}
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class ConsensusStorage(storage: Option[LSMStore]) extends Logging {

  // constant keys for each piece of consensus state
  private val totalStakeKey = ByteArrayWrapper("totalStake".getBytes)
  private val difficultyKey = ByteArrayWrapper("difficulty".getBytes)
  private val inflationKey = ByteArrayWrapper("inflation".getBytes)
  private val heightKey = ByteArrayWrapper("height".getBytes)

  private val defaultTotalStake = 200000000000000000L
  private val defaultDifficulty = 0
  private val defaultInflation = 0
  private val defaultHeight = 0

  private var _totalStake: Int128 = storage match {
    case Some(store) => store.get(totalStakeKey).map(v => Int128(v.data)).getOrElse(defaultTotalStake)
    case None => defaultTotalStake
  }

  private var _difficulty: Long = storage match {
    case Some(store) => store.get(difficultyKey).map(v => Longs.fromByteArray(v.data)).getOrElse(defaultDifficulty)
    case None => defaultDifficulty
  }

  private var _inflation: Int128 = storage match {
    case Some(store) => store.get(inflationKey).map(v => Int128(v.data)).getOrElse(defaultInflation)
    case None => defaultInflation
  }

  private var _height: Int128 = storage match {
    case Some(store) => store.get(heightKey).map(v => Int128(v.data)).getOrElse(defaultHeight)
    case None => defaultHeight
  }

  def totalStake: Int128 = _totalStake
  def difficulty: Long = _difficulty
  def inflation: Int128 = _inflation
  def height: Int128 = _height

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
    val inflationPair = Seq(inflationKey -> params.inflation.toByteArray)
    val heightPair = Seq(heightKey -> params.height.toByteArray)

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
    * @return a Try with a Success result if no errors occurred in the rollback
    */
  def rollbackTo(blockId: ModifierId): Either[NoStorageFailure, Unit] = {
    storage match {
      case Some(store) =>
        Right(store.rollback(toVersionId(blockId)))
      case None =>
        Left(NoStorageFailure())
    }
  }

  /** Converts the given block ID to a version ID to use with LSM Store.
    * @param blockId the ID of the block
    * @return the version ID
    */
  private def toVersionId(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(blockId.getIdBytes)

}

object ConsensusStorage {

  def apply(settings: AppSettings): ConsensusStorage = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val file = new File(s"$dataDir/consensusStorage")
    file.mkdirs()
    val storage = new LSMStore(file)
    val consensusStorage = new ConsensusStorage(Some(storage))

    consensusStorage
  }

  def emptyStorage(): ConsensusStorage = new ConsensusStorage(None)

}

case class ConsensusParams(totalStake: Int128, difficulty: Long, inflation: Int128, height: Int128)

case class NoStorageFailure()
