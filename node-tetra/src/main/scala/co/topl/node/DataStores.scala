package co.topl.node

import cats.{Applicative, MonadThrow}
import cats.data.NonEmptySet
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.BlockHeaderV2Ops
import co.topl.crypto.signing.Ed25519VRF
import co.topl.db.leveldb.LevelDbStore
import co.topl.models._
import co.topl.typeclasses.implicits._
import fs2.io.file.Path

import scala.collection.immutable.ListSet

case class DataStores[F[_]](
  parentChildTree: Store[F, TypedIdentifier, (Long, TypedIdentifier)],
  currentEventIds: Store[F, Byte, TypedIdentifier],
  slotData:        Store[F, TypedIdentifier, SlotData],
  headers:         Store[F, TypedIdentifier, BlockHeaderV2],
  bodies:          Store[F, TypedIdentifier, BlockBodyV2],
  transactions:    Store[F, TypedIdentifier, Transaction],
  spendableBoxIds: Store[F, TypedIdentifier, NonEmptySet[Short]],
  epochBoundaries: Store[F, Long, TypedIdentifier],
  operatorStakes:  Store[F, StakingAddresses.Operator, Int128],
  activeStake:     Store[F, Unit, Int128],
  registrations:   Store[F, StakingAddresses.Operator, Box.Values.Registrations.Operator],
  blockHeightTree: Store[F, Long, TypedIdentifier]
)

object DataStores {

  def init[F[_]: Async](dataDir: Path)(bigBangBlock: BlockV2.Full): F[DataStores[F]] =
    for {
      parentChildTree      <- makeDb[F, TypedIdentifier, (Long, TypedIdentifier)](dataDir)("parent-child-tree")
      currentEventIds      <- makeDb[F, Byte, TypedIdentifier](dataDir)("current-event-ids")
      slotDataStore        <- makeDb[F, TypedIdentifier, SlotData](dataDir)("slot-data")
      blockHeaderStore     <- makeDb[F, TypedIdentifier, BlockHeaderV2](dataDir)("block-headers")
      blockBodyStore       <- makeDb[F, TypedIdentifier, BlockBodyV2](dataDir)("block-bodies")
      transactionStore     <- makeDb[F, TypedIdentifier, Transaction](dataDir)("transactions")
      spendableBoxIdsStore <- makeDb[F, TypedIdentifier, NonEmptySet[Short]](dataDir)("spendable-box-ids")
      epochBoundariesStore <- makeDb[F, Long, TypedIdentifier](dataDir)("epoch-boundaries")
      operatorStakesStore  <- makeDb[F, StakingAddresses.Operator, Int128](dataDir)("operator-stakes")
      activeStakeStore     <- makeDb[F, Unit, Int128](dataDir)("active-stake")
      registrationsStore <- makeDb[F, StakingAddresses.Operator, Box.Values.Registrations.Operator](dataDir)(
        "registrations"
      )
      blockHeightTreeStore <- makeDb[F, Long, TypedIdentifier](dataDir)("block-heights")

      // Store the big bang data
      _ <- currentEventIds
        .contains(0)
        .ifM(
          Applicative[F].unit,
          currentEventIds.put(CurrentEventIdGetterSetters.Indices.CanonicalHead, bigBangBlock.headerV2.id) >>
          List(
            CurrentEventIdGetterSetters.Indices.ConsensusData,
            CurrentEventIdGetterSetters.Indices.EpochBoundaries,
            CurrentEventIdGetterSetters.Indices.BlockHeightTree,
            CurrentEventIdGetterSetters.Indices.BoxState,
            CurrentEventIdGetterSetters.Indices.Mempool
          ).traverseTap(currentEventIds.put(_, bigBangBlock.headerV2.parentHeaderId)).void
        )
      _ <- slotDataStore.put(bigBangBlock.headerV2.id, bigBangBlock.headerV2.slotData(Ed25519VRF.precomputed()))
      _ <- blockHeaderStore.put(bigBangBlock.headerV2.id, bigBangBlock.headerV2)
      _ <- blockBodyStore.put(
        bigBangBlock.headerV2.id,
        ListSet.empty ++ bigBangBlock.transactions.map(_.id.asTypedBytes).toList
      )
      _ <- bigBangBlock.transactions.traverseTap(transaction => transactionStore.put(transaction.id, transaction))
      _ <- blockHeightTreeStore.put(0, bigBangBlock.headerV2.parentHeaderId)
      _ <- activeStakeStore.contains(()).ifM(Applicative[F].unit, activeStakeStore.put((), 0))

      dataStores = DataStores(
        parentChildTree,
        currentEventIds,
        slotDataStore,
        blockHeaderStore,
        blockBodyStore,
        transactionStore,
        spendableBoxIdsStore,
        epochBoundariesStore,
        operatorStakesStore,
        activeStakeStore,
        registrationsStore,
        blockHeightTreeStore
      )

    } yield dataStores

  private def makeDb[F[_]: Async, Key: Persistable, Value: Persistable](dataDir: Path)(
    name:                                                                        String
  ): F[Store[F, Key, Value]] =
    LevelDbStore.makeDb[F](dataDir / name) >>= LevelDbStore.make[F, Key, Value]

}

class CurrentEventIdGetterSetters[F[_]: MonadThrow](store: Store[F, Byte, TypedIdentifier]) {
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

}

object CurrentEventIdGetterSetters {

  /**
   * Captures a getter function and a setter function for a particular "Current Event ID"
   * @param get a function which retrieves the current value/ID
   * @param set a function which sets the current value/ID
   */
  case class GetterSetter[F[_]](get: () => F[TypedIdentifier], set: TypedIdentifier => F[Unit])

  object GetterSetter {

    def forByte[F[_]: MonadThrow](store: Store[F, Byte, TypedIdentifier])(byte: Byte): GetterSetter[F] =
      CurrentEventIdGetterSetters.GetterSetter(() => store.getOrRaise(byte), store.put(byte, _))
  }

  object Indices {
    val CanonicalHead: Byte = 0
    val ConsensusData: Byte = 1
    val EpochBoundaries: Byte = 2
    val BlockHeightTree: Byte = 3
    val BoxState: Byte = 4
    val Mempool: Byte = 5
  }
}
