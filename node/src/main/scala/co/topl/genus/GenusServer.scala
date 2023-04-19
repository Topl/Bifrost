package co.topl.genus

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.genusLibrary.interpreter._
import co.topl.genusLibrary.orientDb.OrientDBFactory
import co.topl.grpc.ToplGrpc
import co.topl.node.ApplicationConfig
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._
import scala.concurrent.duration._

object GenusServer {

  def make[F[_]: Async](conf: ApplicationConfig.Genus): Resource[F, Unit] =
    if (conf.enable) for {
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Genus"))
      orientEC <- Resource
        .make(Sync[F].delay(Executors.newSingleThreadExecutor()))(ec => Sync[F].delay(ec.shutdown()))
        .map(ExecutionContext.fromExecutor)
      orientdb <- OrientDBFactory.make[F](conf.orientDbDirectory, conf.orientDbUser, conf.orientDbPassword, orientEC)

      dbTx   <- Resource.make(Async[F].delay(orientdb.getTx))(db => Async[F].delay(db.shutdown()))
      dbNoTx <- Resource.make(Async[F].delay(orientdb.getNoTx))(db => Async[F].delay(db.shutdown()))

      graphBlockInserter <- GraphBlockInserter.make[F](dbTx, orientEC)
      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx, orientEC)
      blockFetcher       <- GraphBlockFetcher.make(vertexFetcher)
      transactionFetcher <- GraphTransactionFetcher.make(vertexFetcher)

      rpcInterpreter   <- ToplGrpc.Client.make[F](conf.rpcNodeHost, conf.rpcNodePort, conf.rpcNodeTls)
      nodeBlockFetcher <- NodeBlockFetcher.make(rpcInterpreter)

      _ <- GenusGrpc.Server
        .serve(conf.rpcHost, conf.rpcPort, blockFetcher, transactionFetcher)
        .evalTap(grpcServer =>
          Logger[F].info(s"RPC Server bound at ${grpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )
      // TODO: Live data
      // Delay replication by 20 seconds to allow Node RPC to launch
      _ <- (fs2.Stream.sleep(20.seconds) >> fs2.Stream
        .force(nodeBlockFetcher.fetch(startHeight = 1, endHeight = Long.MaxValue))
        .evalMap(graphBlockInserter.insert)).compile.drain.background
    } yield ()
    else Resource.unit[F]

}
