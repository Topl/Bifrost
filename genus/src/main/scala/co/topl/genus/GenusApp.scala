package co.topl.genus

import cats.Show
import cats.effect.IO
import cats.implicits.showInterpolator
import co.topl.common.application.{ContainsDebugFlag, ContainsUserConfigs, IOBaseApp, YamlConfig}
import co.topl.grpc.{HealthCheckGrpc, ToplGrpc}
import co.topl.node.services.NodeRpcFs2Grpc
import com.typesafe.config.Config
import mainargs.{arg, main, Flag, ParserForClass}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource
import pureconfig.generic.auto._

object GenusApp
    extends IOBaseApp[GenusArgs, GenusApplicationConfig](
      createArgs = a => IO.delay(GenusArgs.parserArgs.constructOrThrow(a)),
      createConfig = IOBaseApp.createTypesafeConfig(_),
      parseConfig = (args, conf) => IO.delay(GenusApplicationConfig.unsafe(args, conf))
    ) {

  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName("GenusApp")

  override def run(cmdArgs: GenusArgs, config: Config, appConfig: GenusApplicationConfig): IO[Unit] = (
    for {
      _ <- Logger[F].info(show"Genus args=$cmdArgs").toResource
      nodeRpcProxy <- NodeRpcProxy
        .make[IO](appConfig.nodeRpcHost, appConfig.nodeRpcPort, appConfig.nodeRpcTls)
        .flatMap(NodeRpcFs2Grpc.bindServiceResource[IO])
      genus <-
        Genus
          .make[F](
            appConfig.nodeRpcHost,
            appConfig.nodeRpcPort,
            appConfig.nodeRpcTls,
            appConfig.dataDir,
            appConfig.orientDbPassword
          )
      genusServices <-
        GenusGrpc.Server.services(
          genus.blockFetcher,
          genus.transactionFetcher,
          genus.vertexFetcher,
          genus.valueFetcher
        )
      healthCheck <- GenusHealthCheck.make[IO]().map(_.healthChecker).flatMap(HealthCheckGrpc.Server.services[IO])
      _ <- ToplGrpc.Server.serve[IO](appConfig.rpcBindHost, appConfig.rpcBindPort)(
        nodeRpcProxy :: healthCheck ++ genusServices
      )
      _ <- IO.whenA(appConfig.enableReplicator)(Replicator.stream(genus).compile.drain).toResource
    } yield ()
  ).useForever
}

@main
case class GenusArgs(startup: GenusArgs.Startup, runtime: GenusArgs.Runtime)

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

  @main case class Runtime(
    @arg(doc = "The host to bind for the RPC layer (i.e. 0.0.0.0)")
    rpcBindHost: Option[String] = None,
    @arg(doc = "The port to bind for the RPC layer (i.e. 9084)")
    rpcBindPort: Option[Int] = None,
    @arg(doc = "The host for the Node RPC Client (i.e. localhost)")
    nodeRpcHost: Option[String] = None,
    @arg(doc = "The port for the Node RPC Client (i.e. 9084)")
    nodeRpcPort: Option[Int] = None,
    @arg(doc = "Flag indicating if TLS should be used when connecting to the node.")
    nodeRpcTls: Option[Boolean] = None,
    @arg(doc = "Directory to use for the local database")
    dataDir: Option[String] = None,
    @arg(doc = "The password to use when interacting with OrientDB")
    orientDbPassword: Option[String] = None,
    @arg(doc = "Flag indicating if data should be copied from the node to the local database")
    enableReplicator: Option[Boolean] = None
  )

  implicit val parserStartupArgs: ParserForClass[Startup] =
    ParserForClass[Startup]

  implicit val parserRuntimeArgs: ParserForClass[Runtime] =
    ParserForClass[Runtime]

  implicit val parserArgs: ParserForClass[GenusArgs] =
    ParserForClass[GenusArgs]

  implicit val argsContainsUserConfigs: ContainsUserConfigs[GenusArgs] =
    _.startup.config

  implicit val argsContainsDebugFlag: ContainsDebugFlag[GenusArgs] =
    _.startup.debug.value

  implicit val showArgs: Show[GenusArgs] =
    args =>
      show"GenusApplicationConfig(" +
      show"rpcBindHost=${args.runtime.rpcBindHost}" +
      show" rpcBindPort=${args.runtime.rpcBindPort}" +
      show" nodeRpcHost=${args.runtime.nodeRpcHost}" +
      show" nodeRpcPort=${args.runtime.nodeRpcPort}" +
      show" dataDir=${args.runtime.dataDir}" +
      show" enableReplicator=${args.runtime.enableReplicator}" +
      // NOTE: Do not show orientDbPassword
      show")"
}

case class GenusApplicationConfig(
  rpcBindHost:      String = "0.0.0.0",
  rpcBindPort:      Int = 9084,
  nodeRpcHost:      String = "localhost",
  nodeRpcPort:      Int = 9084,
  nodeRpcTls:       Boolean = false,
  dataDir:          String,
  orientDbPassword: String,
  enableReplicator: Boolean = false
)

object GenusApplicationConfig {

  def unsafe(args: GenusArgs, config: Config): GenusApplicationConfig = {
    val argsAsConfig = {
      val entries = List(
        args.runtime.rpcBindHost.map("rpc-bind-host: " + _),
        args.runtime.rpcBindPort.map("rpc-bind-port: " + _),
        args.runtime.nodeRpcHost.map("node-rpc-host: " + _),
        args.runtime.nodeRpcPort.map("node-rpc-port: " + _),
        args.runtime.nodeRpcTls.map("node-rpc-tls: " + _),
        args.runtime.dataDir.map("data-dir: " + _),
        args.runtime.orientDbPassword.map("orient-db-password: " + _),
        args.runtime.enableReplicator.map("enable-replicator: " + _)
      ).flatten
      YamlConfig.parse(entries.mkString("\n"))
    }

    ConfigSource.fromConfig(config.withFallback(argsAsConfig)).loadOrThrow[GenusApplicationConfig]
  }

  implicit val showApplicationConfig: Show[GenusApplicationConfig] =
    config =>
      show"GenusApplicationConfig(" +
      show"rpcBindHost=${config.rpcBindHost}" +
      show" rpcBindPort=${config.rpcBindPort}" +
      show" nodeRpcHost=${config.nodeRpcHost}" +
      show" nodeRpcPort=${config.nodeRpcPort}" +
      show" dataDir=${config.dataDir}" +
      show" enableReplicator=${config.enableReplicator}" +
      // NOTE: Do not show orientDbPassword
      show")"
}
