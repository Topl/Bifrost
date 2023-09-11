package co.topl.node

import cats.effect.{Async, Sync}
import cats.effect.std.Console
import cats.implicits._
import fs2.Chunk
import fs2.io.file.{Files, Path}

package object cli {

  def writeMessage[F[_]: Sync: Console](message: String): StageResultT[F, Unit] =
    StageResultT.liftF[F, Unit](Sync[F].defer(Console[F].println(message)))

  /**
   * Prompt a user for input and trim any excess spaces
   */
  def readInput[F[_]: Sync: Console]: StageResultT[F, String] =
    StageResultT.liftF(Sync[F].defer(Console[F].print("> ") >> Console[F].readLine.map(_.trim)))

  /**
   * Prompt a user for input trim any excess spaces, and lowercase the result
   */
  def readLowercasedInput[F[_]: Sync: Console]: StageResultT[F, String] =
    readInput.map(_.toLowerCase)

  /**
   * Prompts a user for an enumeration of inputs, and repeats the process until a valid input is received.
   * @param prompt The message to give to the user
   * @param choices A list of (lowercase) choices
   * @param default An optional default value to use if no value is provided.
   *                If the default is part of `choices`, it will be uppercased in the printed message
   * @return The user's selection
   */
  def readLowercasedChoice[F[_]: Sync: Console](
    prompt: String
  )(choices: List[String], default: Option[String] = None): StageResultT[F, String] = {
    val stringifiedChoices =
      choices.map(c => if (default.contains(c)) c.toUpperCase else c).map(c => s" $c ").mkString("[", "|", "]")
    (writeMessage[F](s"$prompt $stringifiedChoices") >> readLowercasedInput[F])
      .flatMap(response =>
        if (response.isEmpty) {
          default.fold(writeMessage[F]("No input provided.").as(none[String]))(d => StageResultT.liftF(d.some.pure[F]))
        } else if (choices.contains(response)) StageResultT.liftF(response.some.pure[F])
        else writeMessage[F](s"Invalid input: $response").as(none[String])
      )
      .untilDefinedM
  }

  /**
   * Write the given file to disk, and log the operation.  Files are saved to the configured staking directory.
   *
   * @param contents The data to write
   * @param logName  The name to use in the log operation
   * @param fileName The name of the file to save (within the staking directory)
   */
  def writeFile[F[_]: Async: Console](
    directory: Path
  )(contents: Array[Byte])(logName: String, fileName: String): StageResultT[F, Unit] =
    StageResultT.liftF[F, Unit] {
      val destination = directory / fileName
      Console[F].println(show"Writing $logName to $destination") >>
      Files.forAsync[F].createDirectories(directory) >>
      fs2.Stream
        .chunk(Chunk.array(contents))
        .through(Files.forAsync[F].writeAll(destination))
        .compile
        .drain
    }
}
