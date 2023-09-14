package co.topl.node.cli

import cats.effect.Async
import cats.effect.std.Console

object QuitCommand {

  def apply[F[_]: Async: Console]: StageResultT[F, Unit] = new QuitCommandImpl[F].command

}

class QuitCommandImpl[F[_]: Async: Console] {

  val command: StageResultT[F, Unit] =
    writeMessage[F]("Mischief Managed.").subflatMap(_ => StageResult.exit)
}
