package co.topl.genus

import cats.data.OptionT
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockId
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.interpreter._
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.OrientDBFactory
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.grpc.ToplGrpc
import co.topl.node.ApplicationConfig
import co.topl.typeclasses.implicits.showBlockId
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._
import scala.concurrent.duration._

object GenusServer {

  def make[F[_]: Async](conf: ApplicationConfig.Genus): Resource[F, Unit] =
    if (conf.enable) for {
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Genus"))
      // A dedicated single thread executor in which all OrientDB calls are expected to run
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
      orientdb <- OrientDBFactory.make[F](conf.orientDbDirectory, conf.orientDbUser, conf.orientDbPassword)

      dbTx   <- Resource.make(Async[F].delay(orientdb.getTx))(db => orientThread.delay(db.shutdown()))
      dbNoTx <- Resource.make(Async[F].delay(orientdb.getNoTx))(db => orientThread.delay(db.shutdown()))

      graphBlockInserter <- GraphBlockInserter.make[F](dbTx)
      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx)
      blockFetcher       <- GraphBlockFetcher.make(vertexFetcher)
      transactionFetcher <- GraphTransactionFetcher.make(vertexFetcher)

      rpcInterpreter   <- ToplGrpc.Client.make[F](conf.rpcNodeHost, conf.rpcNodePort, conf.rpcNodeTls)
      nodeBlockFetcher <- NodeBlockFetcher.make(rpcInterpreter)

      _ <- GenusGrpc.Server
        .serve(conf.rpcHost, conf.rpcPort, blockFetcher, transactionFetcher)
        .evalTap(grpcServer =>
          Logger[F].info(s"RPC Server bound at ${grpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )

      // Delay replication by 20 seconds to allow Node RPC to launch
      processorStream: fs2.Stream[F, Unit] =
        for {
          _ <- fs2.Stream.sleep[F](20.seconds)
          nodeLatestHeight <- fs2.Stream.eval(
            OptionT(nodeBlockFetcher.fetchHeight()).getOrRaise(new IllegalStateException("Unknown node height"))
          )
          graphCurrentHeight <- fs2.Stream.eval(
            OptionT(
              blockFetcher
                .fetchCanonicalHead()
                .rethrow
            ).fold(0L)(_.height)
          )
          _ <- fs2.Stream.eval(
            Logger[F].info(s"Historical data start=${graphCurrentHeight + 1}, end=${nodeLatestHeight}")
          )
          // Historical + live data streams
          _ <- fs2.Stream
            .force[F, BlockData](
              nodeBlockFetcher.fetch(startHeight = graphCurrentHeight + 1, endHeight = nodeLatestHeight + 3)
            )
            .evalTap(blockData => Logger[F].info(s"Inserting block data ${blockData.header.id.show}"))
            .evalMap(graphBlockInserter.insert)
            .rethrow ++
          fs2.Stream
            .force[F, BlockId](
              nodeBlockFetcher.fetchAdoptions()
            )
            .evalMap(nodeBlockFetcher.fetch)
            .rethrow
            .evalTap(blockData => Logger[F].info(s"Inserting block data ${blockData.header.id.show}"))
            .evalMap(graphBlockInserter.insert)
            .recover(_ => ().asRight[GE])

        } yield ()
      _ <- processorStream.compile.drain.background
    } yield ()
    else Resource.unit[F]

}
