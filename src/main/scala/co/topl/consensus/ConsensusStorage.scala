package co.topl.consensus

import java.io.File
import co.topl.modifier.ModifierId
import co.topl.settings.AppSettings
import co.topl.utils.Int128
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

class ConsensusStorage(val storage: LSMStore) {

  // constant keys for each piece of consensus state
  private val totalStakeKey = ByteArrayWrapper("totalStake".getBytes)
  private val difficultyKey = ByteArrayWrapper("difficulty".getBytes)
  private val inflationKey = ByteArrayWrapper("inflation".getBytes)
  private val heightKey = ByteArrayWrapper("height".getBytes)

  /** Updates the consensus storage with the given values at a given block as the version.
    * @param blockId the block ID associated with the consensus parameters
    * @param params the current state of the parameters
    */
  def updateConsensusStorage(blockId: ModifierId, params: ConsensusParams): Unit = {
    // TODO: Branden - determine if this should be blockId.bytes or blockId.getIdBytes
    val versionId = toVersionId(blockId)

    val totalStakePair = Seq(totalStakeKey -> params.totalStake.toByteArray)
    val difficultyPair = Seq(difficultyKey -> params.difficulty.toByteArray)
    val inflationPair = Seq(inflationKey -> params.inflation.toByteArray)
    val heightPair = Seq(heightKey -> params.height.toByteArray)

    val toUpdate = (totalStakePair ++ difficultyPair ++ inflationPair ++ heightPair).map{
      case (k, v) => k -> ByteArrayWrapper(v)
    }

    storage.update(versionId, Seq(), toUpdate)
  }

  def totalStake: Option[Int128] =
    storage.get(totalStakeKey).map(v => Int128(v.data))

  def difficulty: Option[Int128] =
    storage.get(difficultyKey).map(v => Int128(v.data))

  def inflation: Option[Int128] =
    storage.get(inflationKey).map(v => Int128(v.data))

  def height: Option[Int128] =
    storage.get(heightKey).map(v => Int128(v.data))

  /** Rolls back the current state of storage to the data within the given version.
    * @param blockId the ID of the block to rollback to
    * @return a Try with a Success result if no errors occurred in the rollback
    */
  def rollbackTo(blockId: ModifierId): Try[Unit] = Try(storage.rollback(toVersionId(blockId)))

  /** Converts the given block ID to a version ID to use with LSM Store.
    * @param blockId the ID of the block
    * @return the version ID
    */
  private def toVersionId(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(blockId.getIdBytes)

}

case class ConsensusParams(totalStake: Int128, difficulty: Int128, inflation: Int128, height: Int128)

object ConsensusStorage {

  /** Initializes a new instance of consensus storage defaulted with the given values at the given block ID.
    * @param blockId the ID of the version
    * @param totalStake the total amount of stake
    * @param difficulty the starting difficulty
    * @param settings the configured app settings
    * @return an initialized ConsensusStorage with default values at the genesis block version
    */
  def initialize(
    blockId: ModifierId,
    totalStake: Int128,
    difficulty: Int128,
    settings: AppSettings
  ): ConsensusStorage = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val file = new File(s"$dataDir/consensusStorage")
    val storage = new LSMStore(file)

    val consensusStorage = new ConsensusStorage(storage)

    val params = ConsensusParams(totalStake, difficulty, 0, 0)
    consensusStorage.updateConsensusStorage(blockId, params)

    consensusStorage
  }

}
