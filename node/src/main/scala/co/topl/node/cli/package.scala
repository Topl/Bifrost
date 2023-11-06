package co.topl.node

import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.brambl.models.LockAddress
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.models.utility._
import co.topl.node.models.Ratio
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import quivr.models.Int128
import simulacrum.typeclass

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

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

  sealed abstract class YesNo
  case object Yes extends YesNo
  case object No extends YesNo

  /**
   * Prompt the user for a Yes-or-No response, and run the corresponding ifYes/ifNo function.
   * @param prompt The message to give to the user
   * @param default An optional yes/no default.  If none, the user will be continually prompted for a valid answer.
   * @param ifYes computation to execute if the user provides Yes
   * @param ifNo computation to execute if the user provides No
   * @return the result of either ifYes or ifNo
   */
  def readYesNo[F[_]: Sync: Console, T](prompt: String, default: Option[YesNo] = None)(
    ifYes: => StageResultT[F, T],
    ifNo:  => StageResultT[F, T]
  ): StageResultT[F, T] =
    readLowercasedChoice(prompt)(
      List("y", "n"),
      default.map {
        case Yes => "y"
        case No  => "n"
      }
    ).flatMap {
      case "y" => ifYes
      case "n" => ifNo
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

  def readParameter[F[_]: Sync: Console, T: UserInputParser](param: String, examples: List[String]): StageResultT[F, T] =
    readOptionalParameter[F, T](param, examples).untilDefinedM

  def readOptionalParameter[F[_]: Sync: Console, T: UserInputParser](
    param:    String,
    examples: List[String]
  ): StageResultT[F, Option[T]] =
    (writeMessage[F](s"Please enter $param parameter. (Ex: ${examples.mkString(", ")})") >>
      readInput[F]
        .flatMap {
          case "" => none[T].some.pure[StageResultT[F, *]]
          case input =>
            EitherT
              .fromEither[StageResultT[F, *]](UserInputParser[T].parse(input))
              .leftSemiflatTap(error => writeMessage(s"Invalid $param. Reason=$error input=$input"))
              .map(_.some)
              .toOption
              .value
        }).untilDefinedM

  def readDefaultedOptional[F[_]: Sync: Console, T: UserInputParser](
    param:    String,
    examples: List[String],
    default:  String
  ): StageResultT[F, T] =
    (writeMessage[F](show"Please enter $param parameter. (Ex: ${examples.mkString(", ")}) (Default: $default)") >>
      readInput[F]
        .map {
          case "" => default
          case t  => t
        }
        .flatMap(input =>
          EitherT
            .fromEither[StageResultT[F, *]](UserInputParser[T].parse(input))
            .leftSemiflatTap(error => writeMessage(s"Invalid $param. Reason=$error input=$input"))
            .toOption
            .value
        )).untilDefinedM

  implicit private[cli] val parseInt: UserInputParser[Int] = (s: String) => s.toIntOption.toRight("Not an Int")

  implicit private[cli] val parseLong: UserInputParser[Long] = (s: String) => s.toLongOption.toRight("Not a Long")

  implicit private[cli] val parseBigInt: UserInputParser[BigInt] = (s: String) =>
    Try(BigInt(s)).toEither.leftMap(_ => "Not a BigInt")

  implicit private[cli] val parseInt128: UserInputParser[Int128] = (s: String) =>
    parseBigInt.parse(s).map(bigInt => bigInt: Int128)

  implicit private[cli] val parseString: UserInputParser[String] = (s: String) =>
    Either.cond(s.nonEmpty, s, "Empty Input")

  implicit private[cli] val parseRatio: UserInputParser[Ratio] =
    (s: String) =>
      s.split("/") match {
        case Array(numerator) => parseInt128.parse(numerator).map(Ratio(_, BigInt(1)))
        case Array(numerator, denominator) =>
          (parseInt128.parse(numerator), parseInt128.parse(denominator)).mapN(Ratio(_, _))
        case _ => Left("Not a Ratio")
      }

  implicit private[cli] val parseDuration: UserInputParser[com.google.protobuf.duration.Duration] =
    (s: String) =>
      Try(scala.concurrent.duration.Duration(s).asInstanceOf[FiniteDuration]).toEither
        .leftMap(_ => "Not a Duration")
        .map(d => d: com.google.protobuf.duration.Duration)

  implicit private[cli] val parseLockAddress: UserInputParser[LockAddress] =
    (s: String) => co.topl.brambl.codecs.AddressCodecs.decodeAddress(s).leftMap(_.toString)

  implicit private[cli] val parseBlockId: UserInputParser[BlockId] =
    (s: String) => {
      val withoutPrefix = if (s.startsWith("b_")) s.substring(2) else s
      co.topl.brambl.utils.Encoding
        .decodeFromBase58(withoutPrefix)
        .leftMap(_.toString)
        .ensure("Invalid Block ID")(_.length == 32)
        .map(ByteString.copyFrom)
        .map(BlockId(_))
    }
}

@typeclass
trait UserInputParser[T] {
  def parse(input: String): Either[String, T]
}
