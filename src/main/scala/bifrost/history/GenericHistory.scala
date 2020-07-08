package bifrost.history

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.network.SyncInfo
import bifrost.nodeView.NodeViewModifier.{ModifierId, ModifierTypeId}
import bifrost.nodeView.{NodeViewComponent, PersistentNodeViewModifier}
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * History of a blockchain system is some blocktree in fact
  * (like this: http://image.slidesharecdn.com/sfbitcoindev-chepurnoy-2015-150322043044-conversion-gate01/95/proofofstake-its-improvements-san-francisco-bitcoin-devs-hackathon-12-638.jpg),
  * where longest chain is being considered as canonical one, containing right kind of history.
  *
  * In cryptocurrencies of today blocktree view is usually implicit, means code supports only linear history,
  * but other options are possible.
  *
  * To say "longest chain" is the canonical one is simplification, usually some kind of "cumulative difficulty"
  * function has been used instead, even in PoW systems.
  */

trait GenericHistory[P <: Proposition,
TX <: GenericTransaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
HT <: GenericHistory[P, TX, PM, SI, HT]] extends NodeViewComponent {

  import GenericHistory._
  import bifrost.nodeView.NodeViewModifier.ModifierId

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean

  /**
    * Whether the history contains the given modifier
    *
    * @param persistentModifier - modifier
    * @return
    */
  def contains(persistentModifier: PM): Boolean = contains(persistentModifier.id)

  /**
    * Whether the history contains a modifier with the given id
    *
    * @param id - modifier's id
    * @return
    */
  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  /**
    * Whether a modifier could be applied to the history
    *
    * @param modifier - modifier to apply
    * @return
    */
  def applicable(modifier: PM): Boolean = openSurfaceIds().exists(_ sameElements modifier.parentId)

  def modifierById(modifierId: ModifierId): Option[PM]

  def modifierById(modifierId: String): Option[PM] = Base58.decode(modifierId).toOption.flatMap(modifierById)

  def append(modifier: PM): Try[(HT, ProgressInfo[PM])]

  def drop(modifierId: ModifierId): HT

  //todo: output should be ID | Seq[ID]
  def openSurfaceIds(): Seq[ModifierId]

  //todo: argument should be ID | Seq[ID]
  def continuation(from: ModifierIds, size: Int): Option[Seq[PM]] = {
    continuationIds(from, size).map { ids =>
      ids.map(_._2).flatMap(id => modifierById(id))
    }
  }

  //todo: argument should be ID | Seq[ID]
  def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds]

  def syncInfo(answer: Boolean): SI

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(other: SI): HistoryComparisonResult.Value
}

object GenericHistory {

  type ModifierIds = Seq[(ModifierTypeId, ModifierId)]

  object HistoryComparisonResult extends Enumeration {
    val Equal = Value(1)
    val Younger = Value(2)
    val Older = Value(3)
    val Nonsense = Value(4)
  }

  case class ProgressInfo[PM <: PersistentNodeViewModifier[_, _]](branchPoint: Option[ModifierId],
                                                                  toRemove: Seq[PM],
                                                                  toApply: Seq[PM]) {

    require(branchPoint.isDefined == toRemove.nonEmpty)

    lazy val rollbackNeeded = toRemove.nonEmpty
    lazy val appendedId = toApply.last.id

    override def toString: String = {
      s"Modifications(${branchPoint.map(Base58.encode)}, ${toRemove.map(_.encodedId)}, ${toApply.map(_.encodedId)})"
    }
  }

}