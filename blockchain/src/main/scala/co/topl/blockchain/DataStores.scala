package co.topl.blockchain

import cats.MonadThrow
import cats.data.NonEmptySet
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models._
import co.topl.models.Epoch
import co.topl.networking.fsnetwork.KnownRemotePeer
import co.topl.node.models._
import co.topl.proto.node.EpochData
import fs2.io.file.Path

case class DataStores[F[_]](
  baseDirectory:                Path,
  parentChildTree:              Store[F, BlockId, (Long, BlockId)],
  currentEventIds:              Store[F, Byte, BlockId],
  slotData:                     Store[F, BlockId, SlotData],
  headers:                      Store[F, BlockId, BlockHeader],
  bodies:                       Store[F, BlockId, BlockBody],
  transactions:                 Store[F, TransactionId, IoTransaction],
  spendableBoxIdsLocal:         Store[F, TransactionId, NonEmptySet[Short]],
  spendableBoxIdsP2P:           Store[F, TransactionId, NonEmptySet[Short]],
  epochBoundariesLocal:         Store[F, Long, BlockId],
  epochBoundariesP2P:           Store[F, Long, BlockId],
  operatorStakesLocal:          Store[F, StakingAddress, BigInt],
  operatorStakesP2P:            Store[F, StakingAddress, BigInt],
  activeStakeLocal:             Store[F, Unit, BigInt],
  activeStakeP2P:               Store[F, Unit, BigInt],
  inactiveStakeLocal:           Store[F, Unit, BigInt],
  inactiveStakeP2P:             Store[F, Unit, BigInt],
  registrationsLocal:           Store[F, StakingAddress, ActiveStaker],
  registrationsP2P:             Store[F, StakingAddress, ActiveStaker],
  blockHeightTreeLocal:         Store[F, Long, BlockId],
  blockHeightTreeP2P:           Store[F, Long, BlockId],
  epochData:                    Store[F, Epoch, EpochData],
  registrationAccumulatorLocal: Store[F, StakingAddress, Unit],
  registrationAccumulatorP2P:   Store[F, StakingAddress, Unit],
  knownHosts:                   Store[F, Unit, Seq[KnownRemotePeer]],
  metadata:                     Store[F, Array[Byte], Array[Byte]],
  txIdToBlockId:                Store[F, TransactionId, BlockId]
)

/**
 * Data stores which are used during pruning data stores
 */
case class PrunedDataStores[F[_]](
  baseDirectory:        Path,
  parentChildTree:      Store[F, BlockId, (Long, BlockId)],
  slotData:             Store[F, BlockId, SlotData],
  headers:              Store[F, BlockId, BlockHeader],
  bodies:               Store[F, BlockId, BlockBody],
  transactions:         Store[F, TransactionId, IoTransaction],
  blockHeightTreeLocal: Store[F, Long, BlockId],
  blockHeightTreeP2P:   Store[F, Long, BlockId],
  txIdToBlockId:        Store[F, TransactionId, BlockId]
)

class CurrentEventIdGetterSetters[F[_]: MonadThrow](store: Store[F, Byte, BlockId]) {
  import CurrentEventIdGetterSetters.Indices

  val canonicalHead: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.CanonicalHead)

  val consensusDataLocal: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.ConsensusDataLocal)

  val consensusDataP2P: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.ConsensusDataP2P)

  val epochBoundariesLocal: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.EpochBoundariesLocal)

  val epochBoundariesP2P: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.EpochBoundariesP2P)

  val blockHeightTreeLocal: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.BlockHeightTreeLocal)

  val blockHeightTreeP2P: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.BlockHeightTreeP2P)

  val boxStateLocal: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.BoxStateLocal)

  val boxStateP2P: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.BoxStateP2P)

  val mempool: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.Mempool)

  val epochData: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.EpochData)

  val registrationAccumulatorLocal: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.RegistrationAccumulatorLocal)

  val registrationAccumulatorP2P: CurrentEventIdGetterSetters.GetterSetter[F] =
    CurrentEventIdGetterSetters.GetterSetter.forByte(store)(Indices.RegistrationAccumulatorP2P)
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
    val ConsensusDataLocal: Byte = 1
    val EpochBoundariesLocal: Byte = 2
    val BlockHeightTreeLocal: Byte = 3
    val BoxStateLocal: Byte = 4
    val Mempool: Byte = 5
    val EpochData: Byte = 6
    val RegistrationAccumulatorLocal: Byte = 7
    val ConsensusDataP2P: Byte = 8
    val EpochBoundariesP2P: Byte = 9
    val BlockHeightTreeP2P: Byte = 10
    val BoxStateP2P: Byte = 11
    val RegistrationAccumulatorP2P: Byte = 12
  }
}
