package co.topl.consensus

import co.topl.models._

import java.nio.file.Path
import scala.collection.mutable

trait ChainSelectionStorage {
  def currentEpoch: Epoch
  def epochNonces: mutable.Map[Epoch, Nonce]
  def epochTotalStake: mutable.Map[Epoch, Int128]
  def epochStakeDistribution: mutable.Map[Epoch, mutable.Map[Address, Int128]]
}

class LDBMap[Key, Value](path: Path) extends mutable.Map[Key, Value] {
  override def get(key: Key): Option[Value] = ???

  override def iterator: Iterator[(Key, Value)] = ???

  override def addOne(elem: (Key, Value)): LDBMap.this.type = ???

  override def subtractOne(elem: Key): LDBMap.this.type = ???
}

class LDBChainSelectionStorage(path: Path) extends ChainSelectionStorage {

  private var maps: mutable.Map[String, LDBMap[_, _]] =
    mutable.Map.empty

  private def mapNamed[Key, Value](name: String) =
    maps(name).asInstanceOf[LDBMap[Key, Value]]

  override def currentEpoch: Epoch =
    BigInt(mapNamed[String, Bytes]("meta")("epoch").toArray[Byte]).longValue

  override def epochNonces: mutable.Map[Epoch, Nonce] =
    mapNamed[Epoch, Nonce]("epoch_nonces")

  override def epochTotalStake: mutable.Map[Epoch, Int128] =
    mapNamed[Epoch, Int128]("epoch_total_stakes")

  override def epochStakeDistribution: mutable.Map[Epoch, mutable.Map[Address, Int128]] =
    ???
}
