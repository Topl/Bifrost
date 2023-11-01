package co.topl.common.application

import cats.data.{Chain, EitherT, Nested, OptionT}
import cats.effect._
import cats.implicits._
import cats.kernel.Monoid
import com.typesafe.config.{Config, ConfigFactory}
import fs2.io.file.Files
import fs2.io.net.Network
import org.http4s.client.middleware.FollowRedirect
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig._

/**
 * Assists with constructing applications which use cats-effect  Initializes the runtime and configurations.
 * Then runs the defined IO program before cleaning up.
 * @param createArgs a function which turns stringified command-line args into a structured type
 * @param createConfig a function which creates a HOCON config using the parsed command-line args
 * @param parseConfig a function which create application config for given command line arguments and config
 * @param preInitFunction a function which could be run before init runtime
 * @tparam CmdArgs a type representing the arguments of your program
 */
abstract class IOBaseApp[CmdArgs, AppConfig](
  createArgs:      List[String] => IO[CmdArgs],
  createConfig:    CmdArgs => IO[Config],
  parseConfig:     (CmdArgs, Config) => IO[AppConfig],
  preInitFunction: AppConfig => IO[Unit] = (_: AppConfig) => ().pure[IO]
) {

  type F[A] = IO[A]

  def run(cmdArgs: CmdArgs, config: Config, appConfig: AppConfig): IO[Unit]

  def initialize(args: Array[String]): F[(CmdArgs, Config, AppConfig)] =
    for {
      _args      <- createArgs(args.toList)
      _config    <- createConfig(_args)
      _appConfig <- parseConfig(_args, _config)
      _          <- preInitFunction(_appConfig)
    } yield (_args, _config, _appConfig)

  final def main(args: Array[String]): Unit =
    new IOApp {

      def run(args: List[String]): IO[ExitCode] =
        initialize(args.toArray)
          .flatMap((IOBaseApp.this.run _).tupled)
          .as(ExitCode.Success)
    }.main(args)
}

object IOBaseApp {

  implicit val monoidConfig: Monoid[Config] =
    Monoid.instance(ConfigFactory.empty(), _ withFallback _)

  implicit private val logger: SelfAwareStructuredLogger[IO] =
    Slf4jLogger.getLoggerFromName[IO]("IOBaseApp")

  def createTypesafeConfig[Args: ContainsUserConfigs: ContainsDebugFlag](
    cmdArgs:                       Args,
    configFileEnvironmentVariable: Option[String] = None
  ): IO[Config] =
    for {
      debugConfigs <- Nested(argsToDebugConfigs(cmdArgs)).map(_.config()).value
      environmentConfigs = Chain.fromOption(ConfigSource.resources("environment.conf").config().toOption).map(_.asRight)
      userConfigs <- Nested(argsToUserConfigs(cmdArgs)).map(_.config()).value
      configFromEnvironment <- OptionT
        .fromOption[IO](configFileEnvironmentVariable)
        .flatMapF(createTypesafeConfigFromEnvironmentFile)
        .value
        .map(Chain.fromOption)
      defaultConfigs = Chain(ConfigSource.default).map(_.config())
      allConfigs = debugConfigs ++ environmentConfigs ++ userConfigs ++ configFromEnvironment ++ defaultConfigs
      merged <- IO.fromEither(
        (allConfigs.combineAll).map(_.resolve).leftMap(e => new IllegalArgumentException(e.toString))
      )
    } yield merged

  private def createTypesafeConfigFromEnvironmentFile(
    environmentVariableName: String
  ): IO[Option[ConfigReader.Result[Config]]] =
    OptionT(IO.envForIO.get(environmentVariableName))
      .flatMap(fileName =>
        EitherT(readData(fileName).attempt)
          .leftSemiflatTap(_ =>
            logger.warn(s"Environment configuration file not found at location=$fileName.  Skipping.")
          )
          .toOption
          .map(parseData(fileName))
      )
      .map(_.config())
      .value

  /**
   * Allow the --debug argument to enable the `CONFIG_FORCE_` environment variable syntax from Typesafe Config
   */
  private def argsToDebugConfigs[Args: ContainsDebugFlag](args: Args): IO[Chain[ConfigObjectSource]] =
    Chain
      .fromOption(
        Option.when(ContainsDebugFlag[Args].debugFlagOf(args))(
          ConfigSource.fromConfig(ConfigFactory.systemEnvironmentOverrides())
        )
      )
      .pure[IO]

  /**
   * Load user-defined configuration files from disk/resources
   */
  private def argsToUserConfigs[Args: ContainsUserConfigs](args: Args): IO[Chain[ConfigObjectSource]] =
    Chain
      .fromSeq(ContainsUserConfigs[Args].userConfigsOf(args).reverse)
      .traverse(name => readData(name).map(parseData(name)))

  private def readData(name: String) =
    if (name.startsWith("resource://")) IO.blocking {
      val source = scala.io.Source.fromResource(name)
      try source.mkString
      finally source.close()
    }
    else if (name.startsWith("http://") || name.startsWith("https://")) {
      implicit val networkF: Network[IO] = Network.forIO
      EmberClientBuilder
        .default[IO]
        .build
        .map(FollowRedirect(10))
        .use(_.expect[String](name))
    } else {
      Files.forIO.readUtf8(fs2.io.file.Path(name)).compile.foldMonoid
    }

  private def parseData(name: String)(data: String) =
    if (name.endsWith(".yaml") || name.endsWith(".yml"))
      ConfigSource.fromConfig(YamlConfig.parse(data))
    else
      ConfigSource.string(data)
}

@simulacrum.typeclass
trait ContainsUserConfigs[Args] {
  def userConfigsOf(args: Args): List[String]
}

@simulacrum.typeclass
trait ContainsDebugFlag[Args] {
  def debugFlagOf(args: Args): Boolean
}
