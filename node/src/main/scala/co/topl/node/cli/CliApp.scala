package co.topl.node.cli

import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.config.ApplicationConfig

class ConfiguredCliApp(appConfig: ApplicationConfig) {
  import cats.effect.std.Console

  type F[+A] = IO[A]

  implicit private val c: Console[F] = Console[F]

  val run: F[Unit] = applicationResource.use_

  private val applicationResource: Resource[F, Unit] =
    c.println("Welcome to the Bifrost CLI").toResource >>
    (readUserCommand >>= handleUserCommand).value.iterateWhile(_ != StageResult.Exit).void.toResource

  private val readUserCommand =
    StageResultT[F, CliApp.Command](
      (
        c.println("Please enter a command. [ QUIT | register | proposal]") >>
        readLowercaseInput
      ).flatMap {
        case "" | "quit" =>
          CliApp.Command.Quit.some.widen[CliApp.Command].pure[F]
        case "register" =>
          CliApp.Command.Register.some.widen[CliApp.Command].pure[F]
        case "proposal" =>
          CliApp.Command.Proposal.some.widen[CliApp.Command].pure[F]
        case v =>
          c.println(s"Invalid command: `$v`").as(none[CliApp.Command])
      }.untilDefinedM
        .map(StageResult.Success(_))
    )

  private def handleUserCommand(command: CliApp.Command): StageResultT[F, Unit] =
    command match {
      case CliApp.Command.Quit =>
        StageResultT[F, Unit](c.println("Mischief Managed.").as(StageResult.Exit))
      case CliApp.Command.Register =>
        new RegistrationCommand[F](appConfig).command
      case CliApp.Command.Proposal =>
        ProposalCommandImpl.make.command
    }

}

object CliApp {
  sealed abstract class Command

  object Command {
    case object Quit extends Command
    case object Register extends Command
    case object Proposal extends Command
  }
}
