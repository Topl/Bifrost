package co.topl.node

import cats.effect.kernel.Async
import cats.effect.{IO, Resource}
import co.topl.config.ApplicationConfig
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.algebras.Store
import co.topl.consensus.models._
import cats.effect.std.Queue
import cats.implicits._
import co.topl.node.models._
import co.topl.models.utility._
import DataStoresInit.DataStoreNames._
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.blockchain.{DataStores, PrunedDataStores}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import fs2.io.file.Files
import fs2.io.file.Path

class PrunedDataStoresApp(appConfig: ApplicationConfig, prunedDataStorePath: String) {
  type F[A] = IO[A]

  private val concurrency: Int = Runtime.getRuntime.availableProcessors()
  private val messageEveryNBlocks: Int = 50000

  def run: F[Unit] = prunedAppResource.use_

  private def prunedAppResource: Resource[F, Unit] =
    for {
      implicit0(syncF: Async[F])   <- Resource.pure(implicitly[Async[F]])
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Bifrost.Prune"))
      _                            <- log("Launching in prune data stores operation mode")

      (bigBangBlock, dataStores) <- DataStoresInit
        .initializeData(appConfig)
        .onError(_ => log(s"Failed to load db from ${appConfig.bifrost.data.directory}"))
      sourcePath  <- Resource.pure(dataStores.baseDirectory)
      _           <- log(show"Successfully loaded blockchain data from $sourcePath")
      _           <- log(show"with genesis block id ${bigBangBlock.header.id}")
      lastBlockId <- dataStores.canonicalHead.toResource
      _           <- log(show"Top of processed chain is: $lastBlockId")
      _           <- checkInitialDbConsistency(dataStores, lastBlockId)

      prunedDataStores <- DataStoresInit
        .createPrunedDataStores(appConfig, prunedDataStorePath)
        .onError(_ => log(s"Failed to create db at $prunedDataStorePath"))
      targetPath <- Resource.pure(prunedDataStores.baseDirectory)
      _          <- log(show"Created folder for pruned database at $targetPath")

      _ <- log(show"Going to prune data and copy to destination folder")
      _ <- pruneAndCopyData(dataStores, prunedDataStores, lastBlockId)
        .onError(_ => log(s"Failed to prune and copy data"))
      _ <- log(show"Data had been pruned and copied to destination folder $targetPath")

      _ <- log(show"Created pruned data for block related data, going to non purged data")
      _ <- copyNonPurgedData(sourcePath, targetPath).onError(_ => log(s"Failed to copy bob pruned data"))
      _ <- log(show"Metadata had been copied to $targetPath")

      _ <- log("Check pruned db data consistency")
      _ <- checkPrunedDbConsistency(prunedDataStores, lastBlockId, bigBangBlock.header.id)
        .onError(_ => log(s"Pruned data is not consistent DO NOT USE IT"))
    } yield ()

  private def log(message: String)(implicit logger: Logger[F]): Resource[F, Unit] =
    logger.info(message).toResource

  private def getBlockIds(
    slotData:    Store[F, BlockId, SlotData],
    lastBlockId: BlockId
  ): fs2.Stream[F, Option[SlotData]] = {
    def blockIds(id: BlockId): fs2.Stream[F, Option[SlotData]] =
      fs2.Stream.eval(slotData.get(id)).flatMap {
        case Some(sd) => fs2.Stream.emit(sd.some) ++ blockIds(sd.parentSlotId.blockId)
        case None     => fs2.Stream.emit(None)
      }

    blockIds(lastBlockId)
  }

  private def checkInitialDbConsistency(dataStores: DataStores[F], lastBlockId: BlockId)(implicit log: Logger[F]) =
    dataStores.headers
      .get(lastBlockId)
      .map(_.isDefined)
      .ifM(
        Logger[F].info(show"$lastBlockId had been found in database"),
        Async[F].delay(
          throw new IllegalStateException(
            show"Provided directory ${dataStores.baseDirectory} does not contain data for $lastBlockId. Aborting"
          )
        )
      )
      .toResource

  private def pruneAndCopyData(
    dataStores:       DataStores[F],
    prunedDataStores: PrunedDataStores[F],
    lastBlockId:      BlockId
  )(implicit async: Async[F], logger: Logger[F]): Resource[F, Unit] =
    for {
      blockIdsQueue <- Queue.bounded[F, Option[SlotData]](concurrency).toResource
      _ <- getBlockIds(dataStores.slotData, lastBlockId).evalMap(blockIdsQueue.offer).compile.drain.background
      res <- fs2.Stream
        .fromQueueNoneTerminated(blockIdsQueue, concurrency)
        .parEvalMap(concurrency)(id => pruneConsumer(dataStores, prunedDataStores)(id))
        .compile
        .drain
        .start
        .toResource
      _ <- res.join.toResource
    } yield ()

  private def pruneConsumer(dataStores: DataStores[F], prunedDataStores: PrunedDataStores[F])(
    slotData: SlotData
  )(implicit log: Logger[F]): F[Unit] = {
    val blockId = slotData.slotId.blockId
    val blockHeight = slotData.height

    {
      if (blockHeight % messageEveryNBlocks == 0)
        Logger[F].info(show"Copy block with height: $blockHeight")
      else
        ().pure[F]
    } >>
    (
      saveData(dataStores.parentChildTree, prunedDataStores.parentChildTree)(blockId),
      saveSlotData(prunedDataStores.slotData, slotData),
      saveData(dataStores.headers, prunedDataStores.headers)(blockId),
      saveData(dataStores.bodies, prunedDataStores.bodies)(blockId),
      saveHeight(prunedDataStores.blockHeightTreeLocal)(slotData),
      saveHeight(prunedDataStores.blockHeightTreeP2P)(slotData)
    ).parTupled.void >>
    saveTransactions(dataStores.transactions, prunedDataStores.transactions, dataStores.bodies)(blockId)
  }

  private def saveSlotData(target: Store[F, BlockId, SlotData], slotData: SlotData): F[Unit] =
    target.put(slotData.slotId.blockId, slotData)

  private def saveData[T](source: Store[F, BlockId, T], target: Store[F, BlockId, T])(blockId: BlockId): F[Unit] =
    for {
      data <- source.getOrRaise(blockId)
      _    <- target.put(blockId, data)
    } yield ()

  private def saveTransactions(
    source:      Store[F, TransactionId, IoTransaction],
    target:      Store[F, TransactionId, IoTransaction],
    bodiesStore: Store[F, BlockId, BlockBody]
  )(blockId: BlockId): F[Unit] =
    for {
      body  <- bodiesStore.getOrRaise(blockId)
      txIds <- body.allTransactionIds.pure[F]
      txs   <- txIds.traverse(id => source.getOrRaise(id).map(tx => (id, tx)))
      _     <- txs.traverse { case (id, tx) => target.put(id, tx) }
    } yield ()

  private def saveHeight(target: Store[F, Long, BlockId])(slotData: SlotData): F[Unit] =
    target.put(slotData.height, slotData.slotId.blockId)

  private def copyNonPurgedData(sourcePath: Path, targetPath: Path): Resource[F, Unit] =
    for {
      _ <- copyDB(sourcePath, targetPath, currentEventIdsDbName)
      _ <- copyDB(sourcePath, targetPath, spendableBoxIdsStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, spendableBoxIdsStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, epochBoundariesStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, epochBoundariesStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, operatorStakesStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, operatorStakesStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, activeStakeStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, activeStakeStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, inactiveStakeStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, inactiveStakeStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, registrationsStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, registrationsStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, epochDataStoreDbName)
      _ <- copyDB(sourcePath, targetPath, registrationAccumulatorStoreLocalDbName)
      _ <- copyDB(sourcePath, targetPath, registrationAccumulatorStoreP2PDbName)
      _ <- copyDB(sourcePath, targetPath, knownRemotePeersStoreDbName)
      _ <- copyDB(sourcePath, targetPath, metadataStoreDbName)
    } yield ()

  private def copyDB(source: Path, target: Path, dbName: String): Resource[F, Unit] =
    copyRecursively(source / dbName, target / dbName).toResource

  private def copyRecursively(source: Path, target: Path)(implicit async: Async[F]): F[Unit] =
    Files[F]
      .walk(source)
      .evalMap { filePath =>
        val targetPath = target.resolve(source.relativize(filePath))
        Files[F].isDirectory(filePath).ifM(Files[F].createDirectories(targetPath), Files[F].copy(filePath, targetPath))
      }
      .compile
      .drain

  private def checkPrunedDbConsistency(
    prunedDataStores: PrunedDataStores[F],
    lastBlockId:      BlockId,
    genesisBlockId:   BlockId
  )(implicit
    async:  Async[F],
    logger: Logger[F]
  ): Resource[F, Unit] =
    for {
      _ <- log(show"Check head block $lastBlockId")
      _ <- prunedDataStores.slotData.getOrRaise(lastBlockId).map(prunedCheckerConsumer(prunedDataStores)).toResource

      blockIdsQueue <- Queue.bounded[F, Option[SlotData]](concurrency).toResource
      _ <- getBlockIds(prunedDataStores.slotData, lastBlockId).evalMap(blockIdsQueue.offer).compile.drain.background
      res <- fs2.Stream
        .fromQueueNoneTerminated(blockIdsQueue, concurrency)
        .parEvalMap(concurrency)(id => prunedCheckerConsumer(prunedDataStores)(id))
        .compile
        .drain
        .start
        .toResource
      _ <- res.join.toResource

      _ <- log(show"Check Genesis block $genesisBlockId")
      _ <- prunedDataStores.slotData.getOrRaise(genesisBlockId).map(prunedCheckerConsumer(prunedDataStores)).toResource
    } yield ()

  private def prunedCheckerConsumer(prunedDataStores: PrunedDataStores[F])(
    slotData: SlotData
  )(implicit log: Logger[F]): F[Unit] = {
    val blockId = slotData.slotId.blockId
    val blockHeight = slotData.height

    {
      if (blockHeight % messageEveryNBlocks == 0)
        Logger[F].info(show"Checked block with height: $blockHeight")
      else
        ().pure[F]
    } >>
    (
      checkData(prunedDataStores.parentChildTree)(blockId)
        .onError(_ => log.info(show"Error in $parentChildTreeDbName")),
      checkData(prunedDataStores.slotData)(blockId)
        .onError(_ => log.info(show"Error in $slotDataStoreDbName")),
      checkData(prunedDataStores.headers)(blockId)
        .onError(_ => log.info(show"Error in $blockHeaderStoreDbName")),
      checkData(prunedDataStores.bodies)(blockId)
        .onError(_ => log.info(show"Error in $blockBodyStoreDbName")),
      checkTransactions(prunedDataStores)(blockId)
        .onError(_ => log.info(show"Error in $transactionStoreDbName")),
      checkHeight(prunedDataStores.blockHeightTreeLocal)(slotData)
        .onError(_ => log.info(show"Error in $blockHeightTreeStoreLocalDbName")),
      checkHeight(prunedDataStores.blockHeightTreeP2P)(slotData)
        .onError(_ => log.info(show"Error in $blockHeightTreeStoreP2PDbName"))
    ).parTupled.void
  }

  private def checkData[T](source: Store[F, BlockId, T])(blockId: BlockId): F[Unit] = source.getOrRaise(blockId).void

  private def checkTransactions(prunedDataStores: PrunedDataStores[F])(blockId: BlockId) =
    for {
      body  <- prunedDataStores.bodies.getOrRaise(blockId)
      txIds <- body.allTransactionIds.pure[F]
      _     <- txIds.traverse(id => prunedDataStores.transactions.getOrRaise(id))
    } yield ()

  private def checkHeight(target: Store[F, Long, BlockId])(slotData: SlotData): F[Unit] =
    target
      .getOrRaise(slotData.height)
      .map(id => if (id != slotData.slotId.blockId) throw new IllegalStateException("Wrong height"))
}
