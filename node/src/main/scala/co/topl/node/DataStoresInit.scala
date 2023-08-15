package co.topl.node

import cats._
import cats.data.NonEmptySet
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.blockchain.{CurrentEventIdGetterSetters, DataStores}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.{GroupId, TransactionId}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.config.ApplicationConfig
import co.topl.consensus.models._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.db.leveldb.LevelDbStore
import co.topl.interpreters.CacheStore
import co.topl.models.utility._
import co.topl.node.models._
import co.topl.proto.node.EpochData
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger

object DataStoresInit {

  def init[F[_]: Async: Logger](appConfig: ApplicationConfig)(bigBangBlock: FullBlock): Resource[F, DataStores[F]] =
    for {
      dataDir <- Resource.pure[F, Path](
        Path(appConfig.bifrost.data.directory) / bigBangBlock.header.id.show
      )
      _ <- Resource.eval(Files.forAsync[F].createDirectories(dataDir))
      _ <- Resource.eval(Logger[F].info(show"Using dataDir=$dataDir"))
      parentChildTree <- makeCachedDb[F, BlockId, ByteString, (Long, BlockId)](dataDir)(
        "parent-child-tree",
        appConfig.bifrost.cache.parentChildTree,
        _.value
      )
      currentEventIds <- makeDb[F, Byte, BlockId](dataDir)("current-event-ids")
      slotDataStore <- makeCachedDb[F, BlockId, ByteString, SlotData](dataDir)(
        "slot-data",
        appConfig.bifrost.cache.slotData,
        _.value
      )
      blockHeaderStore <- makeCachedDb[F, BlockId, ByteString, BlockHeader](dataDir)(
        "block-headers",
        appConfig.bifrost.cache.headers,
        _.value
      )
      blockBodyStore <- makeCachedDb[F, BlockId, ByteString, BlockBody](dataDir)(
        "block-bodies",
        appConfig.bifrost.cache.bodies,
        _.value
      )
      transactionStore <- makeCachedDb[F, TransactionId, ByteString, IoTransaction](dataDir)(
        "transactions",
        appConfig.bifrost.cache.transactions,
        _.value
      )
      spendableBoxIdsStore <- makeCachedDb[F, TransactionId, ByteString, NonEmptySet[Short]](dataDir)(
        "spendable-box-ids",
        appConfig.bifrost.cache.spendableBoxIds,
        _.value
      )
      epochBoundariesStore <- makeCachedDb[F, Long, java.lang.Long, BlockId](dataDir)(
        "epoch-boundaries",
        appConfig.bifrost.cache.epochBoundaries,
        Long.box
      )
      operatorStakesStore <- makeCachedDb[F, StakingAddress, StakingAddress, BigInt](dataDir)(
        "operator-stakes",
        appConfig.bifrost.cache.operatorStakes,
        identity
      )
      activeStakeStore   <- makeDb[F, Unit, BigInt](dataDir)("active-stake")
      inactiveStakeStore <- makeDb[F, Unit, BigInt](dataDir)("inactive-stake")
      registrationsStore <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        ActiveStaker
      ](dataDir)(
        "registrations",
        appConfig.bifrost.cache.registrations,
        identity
      )
      blockHeightTreeStore <- makeCachedDb[F, Long, java.lang.Long, BlockId](dataDir)(
        "block-heights",
        appConfig.bifrost.cache.blockHeightTree,
        Long.box
      )
      epochDataStore <- makeCachedDb[F, Long, java.lang.Long, EpochData](dataDir)(
        "epoch-data",
        appConfig.bifrost.cache.epochData,
        Long.box
      )
      registrationAccumulatorStore <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        Unit
      ](dataDir)(
        "registration-accumulator",
        appConfig.bifrost.cache.registrationAccumulator,
        identity
      )
      groupStore <- makeCachedDb[F, GroupId, ByteString, Value.Group](dataDir)(
        "group",
        appConfig.bifrost.cache.group,
        _.value
      )
      dataStores = DataStores(
        dataDir,
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
        inactiveStakeStore,
        registrationsStore,
        blockHeightTreeStore,
        epochDataStore,
        registrationAccumulatorStore,
        groupStore
      )
      _ <- Resource.eval(initialize(dataStores, bigBangBlock))
    } yield dataStores

  private def makeDb[F[_]: Async, Key: Persistable, Value: Persistable](dataDir: Path)(
    name: String
  ): Resource[F, Store[F, Key, Value]] =
    LevelDbStore.makeDb[F](dataDir / name).evalMap(LevelDbStore.make[F, Key, Value])

  private def makeCachedDb[F[_]: Async, Key: Persistable, CacheKey <: AnyRef, Value: Persistable](dataDir: Path)(
    name:         String,
    cacheConfig:  ApplicationConfig.Bifrost.Cache.CacheConfig,
    makeCacheKey: Key => CacheKey
  ): Resource[F, Store[F, Key, Value]] =
    makeDb[F, Key, Value](dataDir)(name)
      .evalMap(underlying =>
        CacheStore.make[F, Key, CacheKey, Value](
          underlying.pure[F],
          makeCacheKey,
          _.maximumSize(cacheConfig.maximumEntries),
          cacheConfig.ttl
        )
      )

  private def initialize[F[_]: Monad: Logger](dataStores: DataStores[F], bigBangBlock: FullBlock): F[Unit] =
    for {
      // Store the big bang data
      _ <- dataStores.currentEventIds
        .contains(CurrentEventIdGetterSetters.Indices.CanonicalHead)
        .ifM(
          Logger[F].info("Data stores are already initialized") >> Applicative[F].unit,
          Logger[F].info("Initializing data stores") >>
          dataStores.currentEventIds.put(CurrentEventIdGetterSetters.Indices.CanonicalHead, bigBangBlock.header.id) >>
          List(
            CurrentEventIdGetterSetters.Indices.ConsensusData,
            CurrentEventIdGetterSetters.Indices.EpochBoundaries,
            CurrentEventIdGetterSetters.Indices.BlockHeightTree,
            CurrentEventIdGetterSetters.Indices.BoxState,
            CurrentEventIdGetterSetters.Indices.Mempool,
            CurrentEventIdGetterSetters.Indices.EpochData,
            CurrentEventIdGetterSetters.Indices.RegistrationAccumulator
          ).traverseTap(dataStores.currentEventIds.put(_, bigBangBlock.header.parentHeaderId)).void
        )
      _ <- dataStores.slotData.put(
        bigBangBlock.header.id,
        bigBangBlock.header.slotData(Ed25519VRF.precomputed())
      )
      _ <- dataStores.headers.put(bigBangBlock.header.id, bigBangBlock.header)
      _ <- dataStores.bodies.put(
        bigBangBlock.header.id,
        BlockBody(bigBangBlock.fullBody.transactions.map(_.id), bigBangBlock.fullBody.rewardTransaction.map(_.id))
      )
      _ <- bigBangBlock.fullBody.allTransactions.traverseTap(transaction =>
        dataStores.transactions.put(transaction.id, transaction)
      )
      _ <- dataStores.blockHeightTree.put(0, bigBangBlock.header.parentHeaderId)
      _ <- dataStores.activeStake.contains(()).ifM(Applicative[F].unit, dataStores.activeStake.put((), 0))
      _ <- dataStores.inactiveStake.contains(()).ifM(Applicative[F].unit, dataStores.inactiveStake.put((), 0))
      _ <- dataStores.epochData.put(0, EpochData.defaultInstance)
    } yield ()

}
