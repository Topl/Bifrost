package co.topl.genusServer

import cats.Applicative
import cats.effect._
import cats.effect.implicits.effectResourceOps
import cats._
import cats.data.{EitherT, OptionT}
import cats.syntax.all._
import cats.implicits.{catsSyntaxApplicativeId, toFunctorOps}
import co.topl.common.application.IOBaseApp
import co.topl.genusLibrary.Genus
import co.topl.genusLibrary.algebras.{BodyInserter, HeaderInserter, TxInserter}
import co.topl.genusLibrary.interpreter.mediator.GraphHeaderMediator
import co.topl.genusLibrary.interpreter.{GraphBlockInserter, GraphBodyInserter, GraphHeaderInserter, NodeBlockFetcher, NodeBlockSequenceFetcher}
import co.topl.genusLibrary.orientDb.{GenusGraphMetadata, GraphVertexFetcher, OrientDBFacade}
import co.topl.grpc.ToplGrpc
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j._
import scala.annotation.unused
import scala.jdk.CollectionConverters._

/**
 * This will be the Genus server.
 */
object GenusServerApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (_: Args, conf) => ApplicationConfig.unsafe(conf)
    ) {
  implicit val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger

//  def run(args: List[String]): IO[ExitCode] =
//    for {
//      _ <- logger.info("Genus server starting")
//      _ <- doIt(args).handleErrorWith(e => logger.info(e)("Genus server terminating with unexpected error"))
//      _ <- logger.info("Exiting Genus server")
//      // println(s"BuildInfo: ${co.topl.buildinfo.genusServer.BuildInfo.toString}")
//    } yield ExitCode.Success
//
//  def doIt(@unused args: List[String]): IO[Unit] =
//    IO {
//      try
//        Genus.getGenus
//      // TODO Code to run gRPC services goes here
//      finally
//        Genus.shutDown()
//    }

  override def run: IO[Unit] = applicationResource.use_

  private def applicationResource: Resource[F, Unit] =
    for {
      _              <- Logger[F].info("Genus server starting").toResource
      rpcInterpreter <- ToplGrpc.Client.make[F](appConfig.rpcNodeHost, appConfig.rpcNodePort, appConfig.rpcNodeTls)
      blockFetcher   <- Resource.pure(new NodeBlockFetcher[F](rpcInterpreter))
      blockSequenceFetcher <- Resource.pure(new NodeBlockSequenceFetcher[F](blockFetcher))

      genusTry <- Resource.make(Async[F].delay(Genus.getGenus))(_ => Async[F].delay(Genus.shutDown()))
      genus    <- OptionT(genusTry.toOption.pure[F]).getOrRaise(new RuntimeException("ERROR!")).toResource

      vertexFetcher  <- Resource.pure(new GraphVertexFetcher[F](genus.orientDB, GenusGraphMetadata))
      headerMediator <- Resource.pure(new GraphHeaderMediator[F](genus.orientDB, vertexFetcher))
      headerInserter <- Resource.pure(new GraphHeaderInserter[F](genus.orientDB, headerMediator))

//      bodyInserter <-  Resource.pure(new GraphBodyInserter[F]())
//      txInserter <-  Resource.pure(new GraphTxInserter[F])) // TODO, this is not implemented
      graphBlockInserter <- Resource.pure(new GraphBlockInserter[F](headerInserter, null, null))

      inserter <- blockSequenceFetcher.fetch(1).map(_.evalMap(graphBlockInserter.insert)).toResource
      _ <- inserter.take(3).compile.toVector.map(_.map(res => println(res.leftMap(_.toString).swap.getOrElse("OK")))).toResource
//      _ <- inserter.take(3).map(res => println(res.leftMap(_.toString).swap.getOrElse("OK"))).compile.toList.toResource

      _ <- GenusFullBlockGrpc.Server
        .serve(appConfig.rpcHost, appConfig.rpcPort, blockFetcher)
        .evalTap(grpcServer =>
          Logger[F].info(s"RPC Server bound at ${grpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )
      _ <- Resource.never[F, Unit]
    } yield ()

}
