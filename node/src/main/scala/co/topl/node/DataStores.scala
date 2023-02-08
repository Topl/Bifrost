package co.topl.node

import cats.{Applicative, Monad, MonadThrow}
import cats.data.NonEmptySet
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.BlockHeaderOps
import co.topl.crypto.signing.Ed25519VRF
import co.topl.db.leveldb.LevelDbStore
import co.topl.{models => legacyModels}
import legacyModels._
import legacyModels.utility.ReplaceModelUtil
import co.topl.consensus.models.BlockHeader
import co.topl.node.models.BlockBody
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import co.topl.interpreters.CacheStore
import fs2.io.file.{Files, Path}
import scala.collection.immutable.ListSet
import org.typelevel.log4cats.Logger

case class DataStores[F[_]](
  parentChildTree: Store[F, TypedIdentifier, (Long, TypedIdentifier)],
  currentEventIds: Store[F, Byte, TypedIdentifier],
  slotData:        Store[F, TypedIdentifier, SlotData],
  headers:         Store[F, TypedIdentifier, BlockHeader],
  bodies:          Store[F, TypedIdentifier, BlockBody],
  transactions:    Store[F, TypedIdentifier, Transaction], // TODO replace old Transaction model
  spendableBoxIds: Store[F, TypedIdentifier, NonEmptySet[Short]],
  epochBoundaries: Store[F, Long, TypedIdentifier],
  operatorStakes:  Store[F, StakingAddresses.Operator, Int128],
  activeStake:     Store[F, Unit, Int128],
  registrations:   Store[F, StakingAddresses.Operator, Box.Values.Registrations.Operator],
  blockHeightTree: Store[F, Long, TypedIdentifier]
)

object DataStores {

  def init[F[_]: Async: Logger](appConfig: ApplicationConfig)(bigBangBlock: Block.Full): Resource[F, DataStores[F]] =
    for {
      dataDir <- Resource.pure[F, Path](
        Path(appConfig.bifrost.data.directory) / bigBangBlock.header.id.asTypedBytes.show
      )
      _ <- Resource.eval(Files[F].createDirectories(dataDir))
      _ <- Resource.eval(Logger[F].info(show"Using dataDir=$dataDir"))
      parentChildTree <- makeCachedDb[F, TypedIdentifier, Bytes, (Long, TypedIdentifier)](dataDir)(
        "parent-child-tree",
        appConfig.bifrost.cache.parentChildTree,
        _.allBytes
      )
      currentEventIds <- makeDb[F, Byte, TypedIdentifier](dataDir)("current-event-ids")
      slotDataStore <- makeCachedDb[F, TypedIdentifier, Bytes, SlotData](dataDir)(
        "slot-data",
        appConfig.bifrost.cache.slotData,
        _.allBytes
      )
      blockHeaderStore <- makeCachedDb[F, TypedIdentifier, Bytes, BlockHeader](dataDir)(
        "block-headers",
        appConfig.bifrost.cache.headers,
        _.allBytes
      )
      blockBodyStore <- makeCachedDb[F, TypedIdentifier, Bytes, BlockBody](dataDir)(
        "block-bodies",
        appConfig.bifrost.cache.bodies,
        _.allBytes
      )
      transactionStore <- makeCachedDb[F, TypedIdentifier, Bytes, Transaction](dataDir)(
        "transactions",
        appConfig.bifrost.cache.transactions,
        _.allBytes
      )
      spendableBoxIdsStore <- makeCachedDb[F, TypedIdentifier, Bytes, NonEmptySet[Short]](dataDir)(
        "spendable-box-ids",
        appConfig.bifrost.cache.spendableBoxIds,
        _.allBytes
      )
      epochBoundariesStore <- makeCachedDb[F, Long, java.lang.Long, TypedIdentifier](dataDir)(
        "epoch-boundaries",
        appConfig.bifrost.cache.epochBoundaries,
        Long.box
      )
      operatorStakesStore <- makeCachedDb[F, StakingAddresses.Operator, StakingAddresses.Operator, Int128](dataDir)(
        "operator-stakes",
        appConfig.bifrost.cache.operatorStakes,
        identity
      )
      activeStakeStore <- makeDb[F, Unit, Int128](dataDir)("active-stake")
      registrationsStore <- makeCachedDb[
        F,
        StakingAddresses.Operator,
        StakingAddresses.Operator,
        Box.Values.Registrations.Operator
      ](dataDir)(
        "registrations",
        appConfig.bifrost.cache.registrations,
        identity
      )
      blockHeightTreeStore <- makeCachedDb[F, Long, java.lang.Long, TypedIdentifier](dataDir)(
        "block-heights",
        appConfig.bifrost.cache.blockHeightTree,
        Long.box
      )

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
      _ <- Resource.eval(initialize(dataStores, bigBangBlock))
    } yield dataStores

  private def makeDb[F[_]: Async, Key: Persistable, Value: Persistable](dataDir: Path)(
    name:                                                                        String
  ): Resource[F, Store[F, Key, Value]] =
    LevelDbStore.makeDb[F](dataDir / name).evalMap(LevelDbStore.make[F, Key, Value])

  private def makeCachedDb[F[_]: Async, Key: Persistable, CacheKey <: AnyRef, Value: Persistable](dataDir: Path)(
    name:                                                                                                  String,
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

  private def initialize[F[_]: Monad: Logger](dataStores: DataStores[F], bigBangBlock: Block.Full): F[Unit] =
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
      _ <- dataStores.headers.put(bigBangBlock.header.id, bigBangBlock.toFullConsensus.header)
      _ <- dataStores.bodies.put(
        bigBangBlock.header.id,
        BlockBody(
          (ListSet.empty ++ bigBangBlock.transactions
            .map(_.id.asTypedBytes)
            .toList
            .map(ReplaceModelUtil.ioTransaction32)).toSeq
        )
      )
      _ <- bigBangBlock.transactions.traverseTap(transaction =>
        dataStores.transactions.put(transaction.id, transaction)
      )
      _ <- dataStores.blockHeightTree.put(0, bigBangBlock.header.parentHeaderId)
      _ <- dataStores.activeStake.contains(()).ifM(Applicative[F].unit, dataStores.activeStake.put((), 0))
    } yield ()

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
