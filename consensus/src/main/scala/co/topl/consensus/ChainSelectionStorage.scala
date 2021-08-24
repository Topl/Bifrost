package co.topl.consensus

import co.topl.models._

trait ChainSelectionStorage {
  def currentEpoch: Epoch
  def epochNonces: collection.Map[Epoch, Nonce]
  def epochTotalStake: collection.Map[Epoch, Int128]
  def epochStakeDistribution(epoch: Epoch): collection.Map[Address, Int128]
}

class MapChainSelectionStorage(
  metadata:                collection.Map[String, Bytes],
  val epochNonces:         collection.Map[Epoch, Nonce],
  val epochTotalStake:     collection.Map[Epoch, Int128],
  epochStakeDistributionF: Epoch => collection.Map[Address, Int128]
) extends ChainSelectionStorage {

  override def currentEpoch: Epoch =
    BigInt(metadata("epoch").toArray[Byte]).longValue

  override def epochStakeDistribution(epoch: Epoch): collection.Map[Address, Int128] =
    epochStakeDistributionF(epoch)
}
