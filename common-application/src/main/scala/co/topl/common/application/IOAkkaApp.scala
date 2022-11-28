package co.topl.common.application

import akka.actor.typed.ActorSystem
import cats.effect._
import co.topl.catsakka.AkkaCatsRuntime
import com.typesafe.config.Config

/**
 * Assists with constructing applications which use both cats-effect and akka.  Constructs an
 * ActorSystem that will be installed as the cats-effect runtime.  Then runs the defined
 * IO program before cleaning up.
 * @param createArgs a function which turns stringified command-line args into a structured type
 * @param createConfig a function which creates a HOCON config using the parsed command-line args
 * @param parseConfig a function which create application config for given command line arguments and config
 * @param createSystem a function which creates an ActorSystem using the parsed args and config
 * @param preInitFunction a function which could be run before init runtime
 * @tparam CmdArgs a type representing the arguments of your program
 * @tparam Guardian the actor type of the guardian actor
 */
abstract class IOAkkaApp[CmdArgs, AppConfig, Guardian](
  createArgs:      List[String] => CmdArgs,
  createConfig:    CmdArgs => Config,
  parseConfig:     (CmdArgs, Config) => AppConfig,
  createSystem:    (CmdArgs, AppConfig, Config) => ActorSystem[Guardian],
  preInitFunction: AppConfig => Unit = (_: AppConfig) => ()
) extends IOBaseApp[CmdArgs, AppConfig](createArgs, createConfig, parseConfig, preInitFunction) {

  protected[application] var _system: ActorSystem[Guardian] = _

  implicit def system: ActorSystem[Guardian] = _system

  def run: IO[Unit]

  override protected def initRuntime(): Unit = {
    _system = createSystem(args, appConfig, config)
    _ioRuntime = AkkaCatsRuntime(_system).runtime
  }
}
