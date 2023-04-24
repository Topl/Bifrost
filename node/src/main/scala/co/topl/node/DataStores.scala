package co.topl.node

import cats.data.NonEmptySet
import cats.effect.Async
import cats.effect.Resource
import cats.implicits._
import cats.Applicative
import cats.Monad
import cats.MonadThrow
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SignatureKesProduct
import co.topl.consensus.models.StakingAddress
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.SlotData
import co.topl.crypto.signing.Ed25519VRF
import co.topl.db.leveldb.LevelDbStore
import co.topl.interpreters.CacheStore
import co.topl.node.models.BlockBody
import co.topl.node.models.FullBlock
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2.io.file.Files
import fs2.io.file.Path
import org.typelevel.log4cats.Logger

case class DataStores[F[_]](
  baseDirectory:   Path,
  parentChildTree: Store[F, BlockId, (Long, BlockId)],
  currentEventIds: Store[F, Byte, BlockId],
  slotData:        Store[F, BlockId, SlotData],
  headers:         Store[F, BlockId, BlockHeader],
  bodies:          Store[F, BlockId, BlockBody],
  transactions:    Store[F, TransactionId, IoTransaction], // TODO replace old Transaction model
  spendableBoxIds: Store[F, TransactionId, NonEmptySet[Short]],
  epochBoundaries: Store[F, Long, BlockId],
  operatorStakes:  Store[F, StakingAddress, BigInt],
  activeStake:     Store[F, Unit, BigInt],
  registrations:   Store[F, StakingAddress, SignatureKesProduct],
  blockHeightTree: Store[F, Long, BlockId]
)

object DataStores {

  def init[F[_]: Async: Logger](appConfig: ApplicationConfig)(bigBangBlock: FullBlock): Resource[F, DataStores[F]] =
    for {
      dataDir <- Resource.pure[F, Path](
        Path(appConfig.bifrost.data.directory) / bigBangBlock.header.id.show
      )
      _ <- Resource.eval(Files[F].createDirectories(dataDir))
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
      activeStakeStore <- makeDb[F, Unit, BigInt](dataDir)("active-stake")
      registrationsStore <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        SignatureKesProduct
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
        registrationsStore,
        blockHeightTreeStore
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
            CurrentEventIdGetterSetters.Indices.Mempool
          ).traverseTap(dataStores.currentEventIds.put(_, bigBangBlock.header.parentHeaderId)).void
        )
      _ <- dataStores.slotData.put(
        bigBangBlock.header.id,
        bigBangBlock.header.slotData(Ed25519VRF.precomputed())
      )
      _ <- dataStores.headers.put(bigBangBlock.header.id, bigBangBlock.header)
      _ <- dataStores.bodies.put(
        bigBangBlock.header.id,
        BlockBody(bigBangBlock.fullBody.transactions.map(_.id))
      )
      _ <- bigBangBlock.fullBody.transactions.traverseTap(transaction =>
        dataStores.transactions.put(transaction.id, transaction)
      )
      _ <- dataStores.blockHeightTree.put(0, bigBangBlock.header.parentHeaderId)
      _ <- dataStores.activeStake.contains(()).ifM(Applicative[F].unit, dataStores.activeStake.put((), 0))
    } yield ()

}

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
  }
}
