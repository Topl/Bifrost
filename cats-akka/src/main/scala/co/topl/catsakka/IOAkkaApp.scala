package co.topl.catsakka

import akka.actor.typed.ActorSystem
import cats.effect._
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config

/**
 * Assists with constructing applications which use both cats-effect and akka.  Constructs an
 * ActorSystem that will be installed as the cats-effect runtime.  Then runs the defined
 * IO program before cleaning up.
 * @param createArgs a function which turns stringified command-line args into a structured type
 * @param createConfig a function which creates a HOCON config using the parsed command-line args
 * @param createSystem a function which creates an ActorSystem using the parsed args and config
 * @tparam CmdArgs a type representing the arguments of your program
 * @tparam Guardian the actor type of the guardian actor
 */
abstract class IOAkkaApp[CmdArgs, Guardian](
  createArgs:   List[String] => CmdArgs,
  createConfig: CmdArgs => Config,
  createSystem: (CmdArgs, Config) => ActorSystem[Guardian]
) {

  type F[A] = IO[A]

  private var _args: CmdArgs = _
  private var _config: Config = _
  private var _system: ActorSystem[Guardian] = _
  private var _ioApp: IOApp = _
  private var _ioRuntime: IORuntime = _

  implicit def args: CmdArgs = _args
  implicit def config: Config = _config
  implicit def system: ActorSystem[Guardian] = _system

  def run: IO[Unit]

  final def main(args: Array[String]): Unit = {
    _args = createArgs(args.toList)
    _config = createConfig(_args)
    _system = createSystem(_args, _config)
    _ioRuntime = AkkaCatsRuntime(_system).runtime
    _ioApp = new IOApp {
      override protected val runtime: IORuntime = _ioRuntime
      def run(args: List[String]): IO[ExitCode] =
        IOAkkaApp.this.run.as(ExitCode.Success)
    }
    _ioApp.main(Array.empty)
  }
}
