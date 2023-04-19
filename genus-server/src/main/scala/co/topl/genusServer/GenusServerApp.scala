package co.topl.genusServer

import cats.effect._
import cats.effect.implicits.effectResourceOps
import cats.syntax.all._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.common.application.IOBaseApp
import co.topl.genusLibrary.interpreter._
import co.topl.genusLibrary.orientDb.OrientDBFactory
import co.topl.grpc.ToplGrpc
import co.topl.typeclasses.implicits.showBlockId
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j._

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

object GenusServerApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (_: Args, conf) => ApplicationConfig.unsafe(conf)
    ) {

  implicit val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger

  override def run: IO[Unit] = applicationResource.use_

  private def applicationResource: Resource[F, Unit] =
    for {
      _ <- Logger[F].info("Welcome to Genus").toResource
      conf = appConfig.genus
      orientdb <- OrientDBFactory.make[F](conf.orientDbDirectory, conf.orientDbUser, conf.orientDbPassword)

      dbTx   <- Resource.make(Async[F].delay(orientdb.getTx))(db => Async[F].delay(db.shutdown()))
      dbNoTx <- Resource.make(Async[F].delay(orientdb.getNoTx))(db => Async[F].delay(db.shutdown()))

      graphBlockInserter <- GraphBlockInserter.make[F](dbTx)
      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx)
      blockFetcher       <- GraphBlockFetcher.make(vertexFetcher)
      transactionFetcher <- GraphTransactionFetcher.make(vertexFetcher)

      rpcInterpreter   <- ToplGrpc.Client.make[F](conf.rpcNodeHost, conf.rpcNodePort, conf.rpcNodeTls)
      nodeBlockFetcher <- NodeBlockFetcher.make(rpcInterpreter)

      // TODO this is just proof of concept, we need to add a lot of logic here, related to retries and handling errors
      nodeEnabled <- Resource.pure(true) // maybe we can use a config

      nodeLatestHeight <- nodeBlockFetcher
        .fetchHeight()
        .map(_.getOrElse(Integer.MAX_VALUE: Long))
        .toResource

      graphCurrentHead <-
        blockFetcher
          .fetchCanonicalHead()
          .map(_.map(_.map(_.height)))
          .map(
            _.getOrElse(Some(1L))
          )
          .map(_.getOrElse(1L))
          .toResource

      _ <- Logger[F].info(s"Node height: $nodeLatestHeight").toResource
      inserter <- nodeBlockFetcher
        .fetch(startHeight = graphCurrentHead + 1, endHeight = nodeLatestHeight)
        .map(_.spaced(50 millis))
        .map(
          _.evalMap { blockData =>
            Logger[F].info(s"Inserting block data ${blockData.header.id.show}") >>
            graphBlockInserter.insert(blockData)
          }
            .evalTap(res => Logger[F].info(res.leftMap(_.toString).swap.getOrElse("OK")))
        )
        .toResource

      _ <-
        if (nodeEnabled)
          inserter
            .take(Integer.MAX_VALUE)
            .compile
            .toList
            .void
            .toResource
        else Resource.unit[F]

      _ <- GenusGrpc.Server
        .serve(conf.rpcHost, conf.rpcPort, blockFetcher, transactionFetcher)
        .evalTap(grpcServer =>
          Logger[F].info(s"RPC Server bound at ${grpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )
      _ <- Resource.never[F, Unit]
    } yield ()

}
