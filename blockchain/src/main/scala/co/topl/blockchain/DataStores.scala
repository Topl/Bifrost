package co.topl.blockchain

import cats.MonadThrow
import cats.data.NonEmptySet
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models._
import co.topl.models.Epoch
import co.topl.node.models._
import co.topl.proto.node.EpochData
import fs2.io.file.Path

case class DataStores[F[_]](
  baseDirectory:           Path,
  parentChildTree:         Store[F, BlockId, (Long, BlockId)],
  currentEventIds:         Store[F, Byte, BlockId],
  slotData:                Store[F, BlockId, SlotData],
  headers:                 Store[F, BlockId, BlockHeader],
  bodies:                  Store[F, BlockId, BlockBody],
  transactions:            Store[F, TransactionId, IoTransaction],
  spendableBoxIds:         Store[F, TransactionId, NonEmptySet[Short]],
  epochBoundaries:         Store[F, Long, BlockId],
  operatorStakes:          Store[F, StakingAddress, BigInt],
  activeStake:             Store[F, Unit, BigInt],
  inactiveStake:           Store[F, Unit, BigInt],
  registrations:           Store[F, StakingAddress, ActiveStaker],
  blockHeightTree:         Store[F, Long, BlockId],
  epochData:               Store[F, Epoch, EpochData],
  registrationAccumulator: Store[F, StakingAddress, Unit],
  knownHosts:              Store[F, Unit, Seq[KnownHost]]
)

class CurrentEventIdGetterSetters[F[_]: MonadThrow](store: Store[F, Byte, BlockId]) {
  import CurrentEventIdGetterSetters.Indices

  val canonicalHead: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.CanonicalHead)

  val consensusData: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.ConsensusData)

  val epochBoundaries: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.EpochBoundaries)

  val blockHeightTree: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.BlockHeightTree)

  val boxState: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.BoxState)

  val mempool: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.Mempool)

  val epochData: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.EpochData)

  val registrationAccumulator: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.RegistrationAccumulator)
}

object CurrentEventIdGetterSetters {

  /**
   * Captures a getter function and a setter function for a particular "Current Event ID"
   * @param get a function which retrieves the current value/ID
   * @param set a function which sets the current value/ID
   */
  case class GetterSetter[F[_]](get: () => F[BlockId], set: BlockId => F[Unit])

  object GetterSetter {

    def forByte[F[_]: MonadThrow](store: Store[F, Byte, BlockId])(byte: Byte): GetterSetter[F] =
      CurrentEventIdGetterSetters.GetterSetter(() => store.getOrRaise(byte), store.put(byte, _))
  }

  object Indices {
    val CanonicalHead: Byte = 0
    val ConsensusData: Byte = 1
    val EpochBoundaries: Byte = 2
    val BlockHeightTree: Byte = 3
    val BoxState: Byte = 4
    val Mempool: Byte = 5
    val EpochData: Byte = 6
    val RegistrationAccumulator: Byte = 7
  }
}
