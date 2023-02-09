package co.topl.common.application

import cats.data.Chain
import cats.effect._
import cats.effect.unsafe.IORuntime
import cats.implicits._
import cats.kernel.Monoid
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.{ConfigObjectSource, ConfigSource}

import java.nio.file.Paths

/**
 * Assists with constructing applications which use both cats-effect and akka.  Constructs an
 * ActorSystem that will be installed as the cats-effect runtime.  Then runs the defined
 * IO program before cleaning up.
 * @param createArgs a function which turns stringified command-line args into a structured type
 * @param createConfig a function which creates a HOCON config using the parsed command-line args
 * @param parseConfig a function which create application config for given command line arguments and config
 * @param preInitFunction a function which could be run before init runtime
 * @tparam CmdArgs a type representing the arguments of your program
 */
abstract class IOBaseApp[CmdArgs, AppConfig](
  createArgs:      List[String] => CmdArgs,
  createConfig:    CmdArgs => Config,
  parseConfig:     (CmdArgs, Config) => AppConfig,
  preInitFunction: AppConfig => Unit = (_: AppConfig) => ()
) {

  type F[A] = IO[A]

  protected[application] var _args: CmdArgs = _
  protected[application] var _config: Config = _
  protected[application] var _appConfig: AppConfig = _
  protected[application] var _ioApp: IOApp = _
  protected[application] var _ioRuntime: IORuntime = _

  implicit def args: CmdArgs = _args
  implicit def appConfig: AppConfig = _appConfig
  implicit def config: Config = _config

  def run: IO[Unit]

  protected def initRuntime(): Unit =
    _ioRuntime = cats.effect.unsafe.IORuntime.global

  final def main(args: Array[String]): Unit = {
    _args = createArgs(args.toList)
    _config = createConfig(_args)
    _appConfig = parseConfig(_args, _config)
    preInitFunction(_appConfig)
    initRuntime()
    _ioApp = new IOApp {
      override protected val runtime: IORuntime = _ioRuntime
      def run(args: List[String]): IO[ExitCode] =
        IOBaseApp.this.run.as(ExitCode.Success)
    }
    _ioApp.main(Array.empty)
  }
}

object IOBaseApp {

  implicit private val monoidConfig: Monoid[Config] =
    Monoid.instance(ConfigFactory.empty(), _ withFallback _)

  def createTypesafeConfig[Args: ContainsUserConfigs: ContainsDebugFlag](cmdArgs: Args): Config =
    (
      argsToDebugConfigs(cmdArgs).map(_.config()) ++
        Chain(
          ConfigSource
            .resources("environment.conf")
            .config()
            .orElse(ConfigSource.empty.config())
        ) ++
        argsToUserConfigs(cmdArgs).map(_.config()) ++
        Chain(ConfigSource.default).map(_.config())
    ).combineAll match {
      case Right(value) => value.resolve()
      case Left(e)      => throw new IllegalStateException(e.toString)
    }

  /**
   * Allow the --debug argument to enable the `CONFIG_FORCE_` environment variable syntax from Typesafe Config
   */
  private def argsToDebugConfigs[Args: ContainsDebugFlag](args: Args): Chain[ConfigObjectSource] =
    Chain.fromOption(
      Option.when(ContainsDebugFlag[Args].debugFlagOf(args))(
        ConfigSource.fromConfig(ConfigFactory.systemEnvironmentOverrides())
      )
    )

  /**
   * Load user-defined configuration files from disk/resources
   */
  private def argsToUserConfigs[Args: ContainsUserConfigs](args: Args): Chain[ConfigObjectSource] =
    Chain
      .fromSeq(ContainsUserConfigs[Args].userConfigsOf(args).reverse)
      .map(name =>
        if (name.startsWith("resource://")) {
          if (name.endsWith(".yaml") || name.endsWith(".yml"))
            ConfigSource.fromConfig(YamlConfig.loadResource(name))
          else
            ConfigSource.resources(name)
        } else {
          val path = Paths.get(name)
          if (name.endsWith(".yaml") || name.endsWith(".yml"))
            ConfigSource.fromConfig(YamlConfig.load(path))
          else
            ConfigSource.file(path)
        }
      )
}

@simulacrum.typeclass
trait ContainsUserConfigs[Args] {
  def userConfigsOf(args: Args): List[String]
}

@simulacrum.typeclass
trait ContainsDebugFlag[Args] {
  def debugFlagOf(args: Args): Boolean
}
