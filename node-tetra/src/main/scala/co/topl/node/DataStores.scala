package co.topl.node

import cats.Applicative
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
  slotData:               Store[F, TypedIdentifier, SlotData],
  headers:                Store[F, TypedIdentifier, BlockHeaderV2],
  bodies:                 Store[F, TypedIdentifier, BlockBodyV2],
  transactions:           Store[F, TypedIdentifier, Transaction],
  spendableBoxIds:        Store[F, TypedIdentifier, NonEmptySet[Short]],
  epochBoundaries:        Store[F, Long, TypedIdentifier],
  operatorStakes:         Store[F, StakingAddresses.Operator, Int128],
  activeStake:            Store[F, Unit, Int128],
  registrations:          Store[F, StakingAddresses.Operator, Box.Values.Registrations.Operator],
  blockHeightTree:        Store[F, Long, TypedIdentifier],
  blockHeightTreeUnapply: Store[F, TypedIdentifier, Long]
)

object DataStores {

  def init[F[_]: Async](dataDir: Path)(bigBangBlock: BlockV2.Full): F[DataStores[F]] =
    for {
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
      blockHeightTreeStore        <- makeDb[F, Long, TypedIdentifier](dataDir)("block-heights")
      blockHeightTreeUnapplyStore <- makeDb[F, TypedIdentifier, Long](dataDir)("block-heights-unapply")

      // Store the big bang data
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
        slotDataStore,
        blockHeaderStore,
        blockBodyStore,
        transactionStore,
        spendableBoxIdsStore,
        epochBoundariesStore,
        operatorStakesStore,
        activeStakeStore,
        registrationsStore,
        blockHeightTreeStore,
        blockHeightTreeUnapplyStore
      )

    } yield dataStores

  private def makeDb[F[_]: Async, Key: Persistable, Value: Persistable](dataDir: Path)(
    name:                                                                        String
  ): F[Store[F, Key, Value]] =
    LevelDbStore.makeDb[F](dataDir / name) >>= LevelDbStore.make[F, Key, Value]

}
