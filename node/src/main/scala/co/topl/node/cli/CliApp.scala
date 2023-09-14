package co.topl.node.cli

import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.config.ApplicationConfig

class ConfiguredCliApp(appConfig: ApplicationConfig) {
  import cats.effect.std.Console

  type F[+A] = IO[A]

  implicit private val c: Console[F] = Console.make[F]

  def run: F[Unit] = applicationResource.use_

  private val readUserCommand =
    (
      writeMessage[F]("Please enter a command. [ QUIT | register | proposal | init-testnet]") >>
        readLowercasedInput
    ).semiflatMap {
      case "" | "quit" =>
        CliApp.Command.Quit.some.widen[CliApp.Command].pure[F]
      case "register" =>
        CliApp.Command.Register.some.widen[CliApp.Command].pure[F]
      case "proposal" =>
        CliApp.Command.Proposal.some.widen[CliApp.Command].pure[F]
      case "init-testnet" =>
        CliApp.Command.InitTestnet.some.widen[CliApp.Command].pure[F]
      case v =>
        c.println(s"Invalid command: `$v`").as(none[CliApp.Command])
    }.untilDefinedM

  private val applicationResource: Resource[F, Unit] =
    Sync[F]
      .defer(
        (writeMessage[F]("Welcome to the Bifrost CLI") >> readUserCommand >>= handleUserCommand).value
          .iterateWhile(_ != StageResult.Exit)
          .void
      )
      .toResource

  private def handleUserCommand(command: CliApp.Command): StageResultT[F, Unit] =
    command match {
      case CliApp.Command.Quit        => QuitCommand[F]
      case CliApp.Command.Register    => RegistrationCommand[F](appConfig)
      case CliApp.Command.Proposal    => ProposalCommand[F]
      case CliApp.Command.InitTestnet => InitTestnetCommand[F](appConfig)
    }

}

object CliApp {
  sealed abstract class Command

  object Command {
    case object Quit extends Command
    case object Register extends Command
    case object Proposal extends Command
    case object InitTestnet extends Command
  }
}
