package co.topl.genus

import cats.effect._
import co.topl.algebras._
import co.topl.genusLibrary.algebras._
import co.topl.genusLibrary.interpreter._
import co.topl.genusLibrary.orientDb.OrientDBFactory
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.grpc.NodeGrpc
import fs2.io.file.Files
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Captures the interpreters needed to run Genus
 */
case class Genus[F[_], S[_]](
  nodeRpcClient:      NodeRpc[F, S],
  nodeBlockFetcher:   NodeBlockFetcherAlgebra[F, S],
  vertexFetcher:      VertexFetcherAlgebra[F],
  blockFetcher:       BlockFetcherAlgebra[F],
  blockUpdater:       BlockUpdaterAlgebra[F],
  transactionFetcher: TransactionFetcherAlgebra[F]
)

object Genus {

  def make[F[_]: Async: Files](
    nodeRpcHost: String,
    nodeRpcPort: Int,
    dataDir:     String,
    dbPassword:  String
  ): Resource[F, Genus[F, fs2.Stream[F, *]]] =
    for {
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Genus"))
      // A dedicated single thread executor in which all OrientDB calls are expected to run
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
      orientdb                                 <- OrientDBFactory.make[F](dataDir, dbPassword)

      dbTx <- Resource
        .eval(Async[F].delay(orientdb.getTx))
        .evalTap(db => orientThread.delay(db.makeActive()))
      dbNoTx <- Resource
        .eval(Async[F].delay(orientdb.getNoTx))
        .evalTap(db => orientThread.delay(db.makeActive()))

      rpcInterpreter   <- NodeGrpc.Client.make[F](nodeRpcHost, nodeRpcPort, tls = false)
      nodeBlockFetcher <- NodeBlockFetcher.make(rpcInterpreter)

      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx)
      blockFetcher       <- GraphBlockFetcher.make(vertexFetcher)
      graphBlockUpdater  <- GraphBlockUpdater.make[F](dbTx, blockFetcher, nodeBlockFetcher)
      transactionFetcher <- GraphTransactionFetcher.make(vertexFetcher)
    } yield Genus(rpcInterpreter, nodeBlockFetcher, vertexFetcher, blockFetcher, graphBlockUpdater, transactionFetcher)
}
