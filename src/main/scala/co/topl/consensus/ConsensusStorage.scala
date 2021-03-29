package co.topl.consensus

import java.io.File
import co.topl.modifier.ModifierId
import co.topl.settings.AppSettings
import co.topl.utils.Int128
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

class ConsensusStorage(val storage: LSMStore) {

  /** Update consensus storage whenever a new block is appended to the chain
    *
    */
  def updateConsensusStorage(blockId: ModifierId, params: ConsensusParams): Unit = {
    // TODO: Branden - determine if this should be blockId.bytes or blockId.getIdBytes
    val versionId = ByteArrayWrapper(blockId.bytes)

    val totalStakePair = Seq("totalStake".getBytes -> params.totalStake.toByteArray)
    val difficultyPair = Seq("difficulty".getBytes -> params.difficulty.toByteArray)
    val inflationPair = Seq("inflation".getBytes -> params.inflation.toByteArray)
    val heightPair = Seq("height".getBytes -> params.height.toByteArray)

    val toUpdate = (totalStakePair ++ difficultyPair ++ inflationPair ++ heightPair).map{
      case (k, v) => ByteArrayWrapper(k) -> ByteArrayWrapper(v)
    }

    storage.update(versionId, Seq(), toUpdate)
  }

  def totalStake: Option[Int128] =
    storage.get(ByteArrayWrapper("totalStake".getBytes)).map(b => Int128(b.data))

  def difficulty: Option[Int128] =
    storage.get(ByteArrayWrapper("difficulty".getBytes)).map(b => Int128(b.data))

  def inflation: Option[Int128] =
    storage.get(ByteArrayWrapper("inflation".getBytes)).map(b => Int128(b.data))

  def height: Option[Int128] =
    storage.get(ByteArrayWrapper("height".getBytes)).map(b => Int128(b.data))

  def rollbackTo(blockId: ModifierId): Try[Unit] = Try(storage.rollback(ByteArrayWrapper(blockId.bytes)))

}

case class ConsensusParams(totalStake: Int128, difficulty: Int128, inflation: Int128, height: Int128)

object ConsensusStorage {
  def initialize(
    blockId: ModifierId,
    totalStake: Int128,
    initialDifficulty: Int128,
    settings: AppSettings
  ): ConsensusStorage = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    val file = new File(s"$dataDir/consensusStorage")
    val storage = new LSMStore(file)

    val consensusStorage = new ConsensusStorage(storage)

    val params = ConsensusParams(totalStake, initialDifficulty, 0, 0)
    consensusStorage.updateConsensusStorage(blockId, params)

    consensusStorage
  }
}
