package co.topl.genus

import cats.Show
import cats.effect.IO
import cats.implicits.showInterpolator
import co.topl.common.application.{ContainsDebugFlag, ContainsUserConfigs, IOBaseApp}
import co.topl.grpc.{HealthCheckGrpc, ToplGrpc}
import co.topl.node.services.NodeRpcFs2Grpc
import com.typesafe.config.Config
import mainargs.{Flag, ParserForClass, arg, main}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

object GenusApp
    extends IOBaseApp[GenusArgs, GenusApplicationConfig](
      createArgs = a => IO.delay(GenusArgs.parserArgs.constructOrThrow(a)),
      createConfig = IOBaseApp.createTypesafeConfig(_),
      parseConfig = (_, conf) => IO.delay(GenusApplicationConfig.unsafe(conf))
    ) {

  override def run(cmdArgs: GenusArgs, config: Config, appConfig: GenusApplicationConfig): IO[Unit] = (
    for {
      genus <-
        Genus
          .make[F](
            appConfig.nodeRpcHost,
            appConfig.nodeRpcPort,
            appConfig.orientDbDirectory,
            appConfig.orientDbPassword
          )
      genusServices <-
        GenusGrpc.Server.services(
          genus.blockFetcher,
          genus.transactionFetcher,
          genus.vertexFetcher,
          genus.valueFetcher
        )
      nodeRpcProxy <- NodeRpcProxy
        .make[IO](appConfig.nodeRpcHost, appConfig.nodeRpcPort, appConfig.nodeRpcTls)
        .flatMap(NodeRpcFs2Grpc.bindServiceResource[IO])
      healthCheck <- GenusHealthCheck.make[IO]().map(_.healthChecker).flatMap(HealthCheckGrpc.Server.services[IO])
      _ <- ToplGrpc.Server.serve[IO](appConfig.rpcBindHost, appConfig.rpcBindPort)(
        nodeRpcProxy :: healthCheck ++ genusServices
      )
      _ <- IO.whenA(appConfig.enableReplicator)(Replicator.stream(genus).compile.drain).toResource
    } yield ()
  ).useForever
}

@main
case class GenusArgs(startup: GenusArgs.Startup)

object GenusArgs {

  @main
  case class Startup(
    @arg(
      doc = "Zero or more config files (.conf, .json, .yaml) to apply to the node." +
        "  Config files stack such that the last config file takes precedence." +
        "  To specify an internal resource, prefix the value with \"resource://\"."
    )
    config: List[String] = Nil,
    @arg(
      doc = "An optional flag to enable debug mode on this node."
    )
    debug: Flag
  )

  implicit val parserStartupArgs: ParserForClass[Startup] =
    ParserForClass[Startup]

  implicit val parserArgs: ParserForClass[GenusArgs] =
    ParserForClass[GenusArgs]

  implicit val argsContainsUserConfigs: ContainsUserConfigs[GenusArgs] =
    _.startup.config

  implicit val argsContainsDebugFlag: ContainsDebugFlag[GenusArgs] =
    _.startup.debug.value

  implicit val showArgs: Show[GenusArgs] =
    Show.fromToString
}

case class GenusApplicationConfig(
  rpcBindHost:       String = "0.0.0.0",
  rpcBindPort:       Int = 9084,
  nodeRpcHost:       String,
  nodeRpcPort:       Int = 9084,
  nodeRpcTls:        Boolean = false,
  orientDbDirectory: String,
  orientDbPassword:  String,
  enableReplicator:  Boolean = false
)

object GenusApplicationConfig {

  def unsafe(config: Config): GenusApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[GenusApplicationConfig]

  implicit val showApplicationConfig: Show[GenusApplicationConfig] =
    config =>
      show"GenusApplicationConfig(" +
      show"rpcbindHost=${config.rpcBindHost}" +
      show" rpcbindPort=${config.rpcBindPort}" +
      show" nodeRpcHost=${config.nodeRpcHost}" +
      show" nodeRpcPort=${config.nodeRpcPort}" +
      show" orientDbDirectory=${config.orientDbDirectory}" +
      // NOTE: Do not show orientDbPassword
      show")"
}
