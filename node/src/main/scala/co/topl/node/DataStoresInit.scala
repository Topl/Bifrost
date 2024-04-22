package co.topl.node

import cats._
import cats.data.NonEmptySet
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.Store
import co.topl.blockchain.{BigBang, CurrentEventIdGetterSetters, DataStores, PrivateTestnet, PrunedDataStores}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.config.ApplicationConfig
import co.topl.consensus.models._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.db.leveldb.LevelDbStore
import co.topl.interpreters.CacheStore
import co.topl.interpreters.ContainsCacheStore._
import co.topl.models.utility._
import co.topl.networking.fsnetwork.KnownRemotePeer
import co.topl.node.models._
import co.topl.proto.node.EpochData
import com.google.protobuf.ByteString
import fs2.io.file.{Files, Path}
import org.iq80.leveldb.DBFactory
import org.typelevel.log4cats.Logger
import DataStoresInit.DataStoreNames._
import co.topl.consensus.interpreters.BlockHeaderToBodyValidation
import co.topl.typeclasses.implicits._

object DataStoresInit {

  object DataStoreNames {
    val parentChildTreeDbName = "parent-child-tree"
    val currentEventIdsDbName = "current-event-ids"
    val slotDataStoreDbName = "slot-data"
    val blockHeaderStoreDbName = "block-headers"
    val blockBodyStoreDbName = "block-bodies"
    val transactionStoreDbName = "transactions"
    val spendableBoxIdsStoreLocalDbName = "spendable-box-ids"
    val spendableBoxIdsStoreP2PDbName = "spendable-box-ids-p2p"
    val epochBoundariesStoreLocalDbName = "epoch-boundaries"
    val epochBoundariesStoreP2PDbName = "epoch-boundaries-p2p"
    val operatorStakesStoreLocalDbName = "operator-stakes"
    val operatorStakesStoreP2PDbName = "operator-stakes-p2p"
    val activeStakeStoreLocalDbName = "active-stake"
    val activeStakeStoreP2PDbName = "active-stake-p2p"
    val inactiveStakeStoreLocalDbName = "inactive-stake"
    val inactiveStakeStoreP2PDbName = "inactive-stake-p2p"
    val registrationsStoreLocalDbName = "registrations"
    val registrationsStoreP2PDbName = "registrations-p2p"
    val blockHeightTreeStoreLocalDbName = "block-heights"
    val blockHeightTreeStoreP2PDbName = "block-heights-p2p"
    val epochDataStoreDbName = "epoch-data"
    val registrationAccumulatorStoreLocalDbName = "registration-accumulator"
    val registrationAccumulatorStoreP2PDbName = "registration-accumulator-p2p"
    val knownRemotePeersStoreDbName = "known-remote-peers"
    val metadataStoreDbName = "metadata"
    val txIdToBlockIdDbName = "txId-to-BlockId"
  }

  // scalastyle:off method.length
  /**
   * Creates an instance of DataStores which may-or-may-not be initialized.  It is the responsibility of the caller to
   * call `initialize`.
   * @param appConfig the application's config
   * @param genesisId The (expected) genesis block ID, for path interpolation
   */
  def create[F[_]: Async: Logger](appConfig: ApplicationConfig)(genesisId: BlockId): Resource[F, DataStores[F]] =
    for {
      dataDir        <- Path(interpolateBlockId(genesisId)(appConfig.bifrost.data.directory)).pure[F].toResource
      levelDbFactory <- buildLevelDbFactory(appConfig)
      _              <- Files.forAsync[F].createDirectories(dataDir).toResource
      _              <- Logger[F].info(show"Using dataDir=$dataDir").toResource
      parentChildTree <- makeCachedDb[F, BlockId, ByteString, (Long, BlockId)](dataDir, levelDbFactory)(
        parentChildTreeDbName,
        appConfig.bifrost.cache.parentChildTree,
        _.value
      )
      currentEventIds <- makeDb[F, Byte, BlockId](dataDir, levelDbFactory)(currentEventIdsDbName)
      slotDataStore <- makeCachedDbWithContainsCache[F, BlockId, ByteString, SlotData](dataDir, levelDbFactory)(
        slotDataStoreDbName,
        appConfig.bifrost.cache.slotData,
        _.value,
        appConfig.bifrost.cache.containsCacheSize
      )
      blockHeaderStore <- makeCachedDbWithContainsCache[F, BlockId, ByteString, BlockHeader](dataDir, levelDbFactory)(
        blockHeaderStoreDbName,
        appConfig.bifrost.cache.headers,
        _.value,
        appConfig.bifrost.cache.containsCacheSize
      )
      blockBodyStore <- makeCachedDbWithContainsCache[F, BlockId, ByteString, BlockBody](dataDir, levelDbFactory)(
        blockBodyStoreDbName,
        appConfig.bifrost.cache.bodies,
        _.value,
        appConfig.bifrost.cache.containsCacheSize
      )
      transactionStore <- makeCachedDbWithContainsCache[F, TransactionId, ByteString, IoTransaction](
        dataDir,
        levelDbFactory
      )(
        transactionStoreDbName,
        appConfig.bifrost.cache.transactions,
        _.value,
        appConfig.bifrost.cache.containsCacheSize
      )
      spendableBoxIdsStoreLocal <- makeCachedDb[F, TransactionId, ByteString, NonEmptySet[Short]](
        dataDir,
        levelDbFactory
      )(
        spendableBoxIdsStoreLocalDbName,
        appConfig.bifrost.cache.spendableBoxIds,
        _.value
      )
      spendableBoxIdsStoreP2P <- makeCachedDb[F, TransactionId, ByteString, NonEmptySet[Short]](
        dataDir,
        levelDbFactory
      )(
        spendableBoxIdsStoreP2PDbName,
        appConfig.bifrost.cache.spendableBoxIds,
        _.value
      )
      epochBoundariesStoreLocal <- makeCachedDb[F, Long, java.lang.Long, BlockId](dataDir, levelDbFactory)(
        epochBoundariesStoreLocalDbName,
        appConfig.bifrost.cache.epochBoundaries,
        Long.box
      )
      epochBoundariesStoreP2P <- makeCachedDb[F, Long, java.lang.Long, BlockId](dataDir, levelDbFactory)(
        epochBoundariesStoreP2PDbName,
        appConfig.bifrost.cache.epochBoundaries,
        Long.box
      )
      operatorStakesStoreLocal <- makeCachedDb[F, StakingAddress, StakingAddress, BigInt](dataDir, levelDbFactory)(
        operatorStakesStoreLocalDbName,
        appConfig.bifrost.cache.operatorStakes,
        identity
      )
      operatorStakesStoreP2P <- makeCachedDb[F, StakingAddress, StakingAddress, BigInt](dataDir, levelDbFactory)(
        operatorStakesStoreP2PDbName,
        appConfig.bifrost.cache.operatorStakes,
        identity
      )
      activeStakeStoreLocal   <- makeDb[F, Unit, BigInt](dataDir, levelDbFactory)(activeStakeStoreLocalDbName)
      activeStakeStoreP2P     <- makeDb[F, Unit, BigInt](dataDir, levelDbFactory)(activeStakeStoreP2PDbName)
      inactiveStakeStoreLocal <- makeDb[F, Unit, BigInt](dataDir, levelDbFactory)(inactiveStakeStoreLocalDbName)
      inactiveStakeStoreP2P   <- makeDb[F, Unit, BigInt](dataDir, levelDbFactory)(inactiveStakeStoreP2PDbName)
      registrationsStoreLocal <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        ActiveStaker
      ](dataDir, levelDbFactory)(
        registrationsStoreLocalDbName,
        appConfig.bifrost.cache.registrations,
        identity
      )
      registrationsStoreP2P <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        ActiveStaker
      ](dataDir, levelDbFactory)(
        registrationsStoreP2PDbName,
        appConfig.bifrost.cache.registrations,
        identity
      )
      blockHeightTreeStoreLocal <- makeCachedDb[F, Long, java.lang.Long, BlockId](dataDir, levelDbFactory)(
        blockHeightTreeStoreLocalDbName,
        appConfig.bifrost.cache.blockHeightTree,
        Long.box
      )
      blockHeightTreeStoreP2P <- makeCachedDb[F, Long, java.lang.Long, BlockId](dataDir, levelDbFactory)(
        blockHeightTreeStoreP2PDbName,
        appConfig.bifrost.cache.blockHeightTree,
        Long.box
      )
      epochDataStore <- makeCachedDb[F, Long, java.lang.Long, EpochData](dataDir, levelDbFactory)(
        epochDataStoreDbName,
        appConfig.bifrost.cache.epochData,
        Long.box
      )
      registrationAccumulatorStoreLocal <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        Unit
      ](dataDir, levelDbFactory)(
        registrationAccumulatorStoreLocalDbName,
        appConfig.bifrost.cache.registrationAccumulator,
        identity
      )
      registrationAccumulatorStoreP2P <- makeCachedDb[
        F,
        StakingAddress,
        StakingAddress,
        Unit
      ](dataDir, levelDbFactory)(
        registrationAccumulatorStoreP2PDbName,
        appConfig.bifrost.cache.registrationAccumulator,
        identity
      )
      knownRemotePeersStore <- makeDb[F, Unit, Seq[KnownRemotePeer]](dataDir, levelDbFactory)(
        knownRemotePeersStoreDbName
      )
      metadataStore <- makeDb[F, Array[Byte], Array[Byte]](dataDir, levelDbFactory)(metadataStoreDbName)
      txIdToBlockId <- makeCachedDb[F, TransactionId, ByteString, BlockId](dataDir, levelDbFactory)(
        txIdToBlockIdDbName,
        appConfig.bifrost.cache.txIdToBlockId,
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
        spendableBoxIdsStoreLocal,
        spendableBoxIdsStoreP2P,
        epochBoundariesStoreLocal,
        epochBoundariesStoreP2P,
        operatorStakesStoreLocal,
        operatorStakesStoreP2P,
        activeStakeStoreLocal,
        activeStakeStoreP2P,
        inactiveStakeStoreLocal,
        inactiveStakeStoreP2P,
        registrationsStoreLocal,
        registrationsStoreP2P,
        blockHeightTreeStoreLocal,
        blockHeightTreeStoreP2P,
        epochDataStore,
        registrationAccumulatorStoreLocal,
        registrationAccumulatorStoreP2P,
        knownRemotePeersStore,
        metadataStore,
        txIdToBlockId
      )
    } yield dataStores
  // scalastyle:on method.length

  def createPrunedDataStores[F[_]: Async: Logger](
    appConfig:           ApplicationConfig,
    prunedDataStorePath: String
  ): Resource[F, PrunedDataStores[F]] =
    for {
      dataDir         <- Path(prunedDataStorePath).pure[F].toResource
      levelDbFactory  <- buildLevelDbFactory(appConfig)
      files           <- Files.forAsync[F].pure[F].toResource
      _               <- files.exists(dataDir).ifM(files.deleteRecursively(dataDir), ().pure[F]).toResource
      _               <- files.createDirectories(dataDir).toResource
      _               <- Logger[F].info(show"Using dataDir=$dataDir").toResource
      parentChildTree <- makeDb[F, BlockId, (Long, BlockId)](dataDir, levelDbFactory)(parentChildTreeDbName)
      slotDataStore <- makeCachedDb[F, BlockId, ByteString, SlotData](dataDir, levelDbFactory)(
        slotDataStoreDbName,
        appConfig.bifrost.cache.slotData,
        _.value
      )
      // meter <- Meter[F].
      blockHeaderStore <- makeDb[F, BlockId, BlockHeader](dataDir, levelDbFactory)(blockHeaderStoreDbName)
      blockBodyStore <- makeCachedDb[F, BlockId, ByteString, BlockBody](dataDir, levelDbFactory)(
        blockBodyStoreDbName,
        appConfig.bifrost.cache.slotData,
        _.value
      )
      transactionStore <- makeDb[F, TransactionId, IoTransaction](dataDir, levelDbFactory)(transactionStoreDbName)
      blockHeightTreeStoreLocal <- makeDb[F, Long, BlockId](dataDir, levelDbFactory)(blockHeightTreeStoreLocalDbName)
      blockHeightTreeStoreP2P   <- makeDb[F, Long, BlockId](dataDir, levelDbFactory)(blockHeightTreeStoreP2PDbName)
      txIdToBlockId             <- makeDb[F, TransactionId, BlockId](dataDir, levelDbFactory)(txIdToBlockIdDbName)
    } yield PrunedDataStores(
      dataDir,
      parentChildTree,
      slotDataStore,
      blockHeaderStore,
      blockBodyStore,
      transactionStore,
      blockHeightTreeStoreLocal,
      blockHeightTreeStoreP2P,
      txIdToBlockId
    )

  /**
   * Based on the application config, determines if the genesis block is a public network or a private testnet, and
   * initialize it accordingly.  In addition, creates the underlying `DataStores` instance and verifies the stored
   * data against the configured data (if applicable).
   */
  def initializeData[F[_]: Async: Logger](
    appConfig: ApplicationConfig
  ): Resource[F, (FullBlock, DataStores[F])] =
    appConfig.bifrost.bigBang match {
      case privateBigBang: ApplicationConfig.Bifrost.BigBangs.Private =>
        for {
          testnetStakerInitializers <- Sync[F]
            .delay(PrivateTestnet.stakerInitializers(privateBigBang.timestamp, privateBigBang.stakerCount))
            .toResource
          bigBangConfig <- Sync[F]
            .delay(
              PrivateTestnet
                .config(
                  privateBigBang.timestamp,
                  testnetStakerInitializers,
                  privateBigBang.stakes,
                  PrivateTestnet.DefaultProtocolVersion,
                  appConfig.bifrost.protocols(0)
                )
            )
            .toResource
          bigBangBlock = BigBang.fromConfig(bigBangConfig)
          dataStores <- DataStoresInit.create[F](appConfig)(bigBangBlock.header.id)
          _ <- DataStoresInit
            .isInitialized(dataStores)
            .ifM(DataStoresInit.repair(dataStores, bigBangBlock), DataStoresInit.initialize(dataStores, bigBangBlock))
            .toResource
          _ <- privateBigBang.localStakerIndex
            .filter(_ >= 0)
            .traverse(index =>
              PrivateTestnet
                .writeStaker[F](
                  Path(interpolateBlockId(bigBangBlock.header.id)(appConfig.bifrost.staking.directory)),
                  testnetStakerInitializers(index),
                  privateBigBang.stakes.fold(PrivateTestnet.defaultStake(privateBigBang.stakerCount))(_.apply(index))
                )
                .toResource
            )
        } yield (bigBangBlock, dataStores)
      case publicBigBang: ApplicationConfig.Bifrost.BigBangs.Public =>
        DataStoresInit
          .create[F](appConfig)(publicBigBang.genesisId)
          .flatMap(dataStores =>
            DataStoresInit
              .isInitialized(dataStores)
              .toResource
              .ifM(
                for {
                  header       <- dataStores.headers.getOrRaise(publicBigBang.genesisId).toResource
                  body         <- dataStores.bodies.getOrRaise(publicBigBang.genesisId).toResource
                  transactions <- body.transactionIds.traverse(dataStores.transactions.getOrRaise).toResource
                  fullBlock = FullBlock(header, FullBlockBody(transactions))
                  _ <- DataStoresInit.repair[F](dataStores, fullBlock).toResource
                } yield fullBlock,
                DataReaders
                  .fromSourcePath[F](publicBigBang.sourcePath)
                  .use(reader =>
                    BlockHeaderToBodyValidation
                      .make[F]()
                      .flatMap(
                        BigBang.fromRemote(reader)(_)(publicBigBang.genesisId)
                      )
                      .flatTap(DataStoresInit.initialize(dataStores, _))
                  )
                  .toResource
              )
              .tupleRight(dataStores)
          )

    }

  private def buildLevelDbFactory[F[_]: Async: Logger](appConfig: ApplicationConfig): Resource[F, DBFactory] =
    for {
      databaseType <- appConfig.bifrost.data.databaseType
        .pure[F]
        .ensure(new IllegalArgumentException(s"Invalid databaseType=${appConfig.bifrost.data.databaseType}"))(dbType =>
          Set(DatabaseTypes.LevelDbJni, DatabaseTypes.LevelDbJava).contains(dbType)
        )
        .toResource
      levelDbFactory <- LevelDbStore.makeFactory[F](useJni = databaseType == DatabaseTypes.LevelDbJni)
    } yield levelDbFactory

  private def makeDb[F[_]: Async, Key: Persistable, Value: Persistable](dataDir: Path, dbFactory: DBFactory)(
    name: String
  ): Resource[F, Store[F, Key, Value]] =
    LevelDbStore.makeDb[F](dataDir / name, dbFactory).evalMap(LevelDbStore.make[F, Key, Value])

  private def makeCachedDb[F[_]: Async, Key: Persistable, CacheKey <: AnyRef, Value: Persistable](
    dataDir:   Path,
    dbFactory: DBFactory
  )(
    name:         String,
    cacheConfig:  ApplicationConfig.Bifrost.Cache.CacheConfig,
    makeCacheKey: Key => CacheKey
  ): Resource[F, Store[F, Key, Value]] =
    makeDb[F, Key, Value](dataDir, dbFactory)(name)
      .evalMap(underlying =>
        CacheStore.make[F, Key, CacheKey, Value](
          underlying.pure[F],
          makeCacheKey,
          _.maximumSize(cacheConfig.maximumEntries),
          cacheConfig.ttl
        )
      )

  private def makeCachedDbWithContainsCache[F[_]: Async, Key: Persistable, CacheKey <: AnyRef, Value: Persistable](
    dataDir:   Path,
    dbFactory: DBFactory
  )(
    name:              String,
    cacheConfig:       ApplicationConfig.Bifrost.Cache.CacheConfig,
    makeCacheKey:      Key => CacheKey,
    containsCacheSize: Long
  ): Resource[F, Store[F, Key, Value]] =
    makeCachedDb[F, Key, CacheKey, Value](dataDir, dbFactory)(name, cacheConfig, makeCacheKey)
      .evalMap(_.withCachedContains(containsCacheSize))

  /**
   * Determines if the given DataStores have already been initialized (i.e. node re-launch)
   */
  def isInitialized[F[_]: MonadThrow: Logger](
    dataStores: DataStores[F]
  ): F[Boolean] =
    dataStores.currentEventIds
      .contains(CurrentEventIdGetterSetters.Indices.CanonicalHead)
      .flatTap(result =>
        if (result) Logger[F].info("Data stores already initialized")
        else Logger[F].info("Data stores not initialized")
      )

  /**
   * Initializes the given (empty) DataStores with the provided genesis block
   */
  def initialize[F[_]: Sync: Logger](dataStores: DataStores[F], bigBangBlock: FullBlock): F[Unit] =
    for {
      // Store the big bang data
      _ <- Logger[F].info("Initializing data stores")
      _ <- dataStores.currentEventIds.put(CurrentEventIdGetterSetters.Indices.CanonicalHead, bigBangBlock.header.id)
      _ <- List(
        CurrentEventIdGetterSetters.Indices.ConsensusDataLocal,
        CurrentEventIdGetterSetters.Indices.ConsensusDataP2P,
        CurrentEventIdGetterSetters.Indices.EpochBoundariesLocal,
        CurrentEventIdGetterSetters.Indices.EpochBoundariesP2P,
        CurrentEventIdGetterSetters.Indices.BlockHeightTreeLocal,
        CurrentEventIdGetterSetters.Indices.BlockHeightTreeP2P,
        CurrentEventIdGetterSetters.Indices.BoxStateLocal,
        CurrentEventIdGetterSetters.Indices.BoxStateP2P,
        CurrentEventIdGetterSetters.Indices.Mempool,
        CurrentEventIdGetterSetters.Indices.EpochData,
        CurrentEventIdGetterSetters.Indices.RegistrationAccumulatorLocal,
        CurrentEventIdGetterSetters.Indices.RegistrationAccumulatorP2P
      ).traverseTap(dataStores.currentEventIds.put(_, bigBangBlock.header.parentHeaderId))
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
      _ <- dataStores.blockHeightTreeLocal.put(0, bigBangBlock.header.parentHeaderId)
      _ <- dataStores.blockHeightTreeP2P.put(0, bigBangBlock.header.parentHeaderId)
      _ <- dataStores.activeStakeLocal.contains(()).ifM(Applicative[F].unit, dataStores.activeStakeLocal.put((), 0))
      _ <- dataStores.activeStakeP2P.contains(()).ifM(Applicative[F].unit, dataStores.activeStakeP2P.put((), 0))
      _ <- dataStores.inactiveStakeLocal.contains(()).ifM(Applicative[F].unit, dataStores.inactiveStakeLocal.put((), 0))
      _ <- dataStores.inactiveStakeP2P.contains(()).ifM(Applicative[F].unit, dataStores.inactiveStakeP2P.put((), 0))
      _ <- dataStores.epochData.put(0, EpochData.defaultInstance)
      _ <- dataStores.parentChildTree.put(
        bigBangBlock.header.id,
        (bigBangBlock.header.height, bigBangBlock.header.parentHeaderId)
      )
    } yield ()

  def repair[F[_]: Sync](dataStores: DataStores[F], bigBangBlock: FullBlock): F[Unit] =
    for {
      repairEventId <- (
        (key: Byte) =>
          dataStores.currentEventIds
            .contains(key)
            .ifM(().pure[F], dataStores.currentEventIds.put(key, bigBangBlock.header.parentHeaderId))
      ).pure[F]
      _ <- repairEventId(CurrentEventIdGetterSetters.Indices.ConsensusDataP2P)
      _ <- repairEventId(CurrentEventIdGetterSetters.Indices.EpochBoundariesP2P)
      _ <- repairEventId(CurrentEventIdGetterSetters.Indices.BlockHeightTreeP2P)
      _ <- repairEventId(CurrentEventIdGetterSetters.Indices.BoxStateP2P)
      _ <- repairEventId(CurrentEventIdGetterSetters.Indices.RegistrationAccumulatorP2P)
      _ <- dataStores.blockHeightTreeP2P
        .contains(0)
        .ifM(Applicative[F].unit, dataStores.blockHeightTreeP2P.put(0, bigBangBlock.header.parentHeaderId))
      _ <- dataStores.activeStakeP2P.contains(()).ifM(Applicative[F].unit, dataStores.activeStakeP2P.put((), 0))
      _ <- dataStores.inactiveStakeP2P.contains(()).ifM(Applicative[F].unit, dataStores.inactiveStakeP2P.put((), 0))
    } yield ()

  object DatabaseTypes {
    final val LevelDbJni = "levelDb-jni"
    final val LevelDbJava = "levelDb-java"
  }

}
