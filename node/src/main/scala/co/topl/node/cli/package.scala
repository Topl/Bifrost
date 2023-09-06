package co.topl.node

import cats.Monad
import cats.effect.Async
import cats.effect.std.Console
import cats.implicits._
import fs2.Chunk
import fs2.io.file.{Files, Path}

package object cli {

  /**
   * Prompt a user for input and trim any excess spaces
   */
  def readInput[F[_]: Console: Monad]: F[String] =
    Console[F].print("> ") >> Console[F].readLine.map(_.trim)

  /**
   * Prompt a user for input trim any excess spaces, and lowercase the result
   */
  def readLowercaseInput[F[_]: Console: Monad]: F[String] =
    readInput.map(_.toLowerCase)

  /**
   * Write the given file to disk, and log the operation.  Files are saved to the configured staking directory.
   *
   * @param contents The data to write
   * @param logName  The name to use in the log operation
   * @param fileName The name of the file to save (within the staking directory)
   */
  def write[F[_]: Console: Async](directory: Path)(contents: Array[Byte])(logName: String, fileName: String) =
    StageResultT.liftF[F, Unit] {
      val destination = directory / fileName
      Console[F].println(show"Writing $logName to $destination") >>
      destination.parent.traverse(Files.forAsync[F].createDirectories) >>
      fs2.Stream
        .chunk(Chunk.array(contents))
        .through(Files[F].writeAll(destination))
        .compile
        .drain
    }
}
