package co.topl.genusServer

import cats.effect._
import cats.effect.implicits.effectResourceOps
import cats.syntax.all._
import co.topl.common.application.IOBaseApp
import co.topl.genusLibrary.interpreter._
import co.topl.genusLibrary.orientDb.OrientDBFactory
import co.topl.grpc.ToplGrpc
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
      orientdb <- OrientDBFactory
        .make[F](appConfig.orientDbDirectory, appConfig.orientDbUser, appConfig.orientDbPassword)

      dbTx   <- Resource.make(Async[F].delay(orientdb.getTx))(db => Async[F].delay(db.shutdown()))
      dbNoTx <- Resource.make(Async[F].delay(orientdb.getNoTx))(db => Async[F].delay(db.shutdown()))

      graphBlockInserter <- GraphBlockInserter.make[F](dbTx)
      vertexFetcher      <- GraphVertexFetcher.make[F](dbNoTx)

      rpcInterpreter <- ToplGrpc.Client.make[F](appConfig.rpcNodeHost, appConfig.rpcNodePort, appConfig.rpcNodeTls)
      blockFetcher   <- NodeBlockFetcher.make(rpcInterpreter)
      blockSequenceFetcher <- NodeBlockSequenceFetcher.make(blockFetcher)

      // TODO this is just proof of concept, we need to add a lot of logic here, related to retries and handling errors
      inserter <- blockSequenceFetcher
        .fetch(1, 100)
        .map(_.spaced(50 millis))
        .map(
          _.evalMap(graphBlockInserter.insert)
            .evalTap(res => Logger[F].info(res.leftMap(_.toString).swap.getOrElse("OK")))
        )
        .toResource

      _ <- inserter
        .take(50)
        .compile
        .toList
        .toResource

      _ <- GenusFullBlockGrpc.Server
        .serve(appConfig.rpcHost, appConfig.rpcPort, vertexFetcher)
        .evalTap(grpcServer =>
          Logger[F].info(s"RPC Server bound at ${grpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )
      _ <- Resource.never[F, Unit]
    } yield ()

}
