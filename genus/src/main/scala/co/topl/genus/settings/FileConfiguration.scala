package co.topl.genus.settings

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import co.topl.genus.settings.Configuration.{ReadFailure, ReadFailures}
import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import java.io.File
import scala.util.Try

object FileConfiguration {

  /**
   * Creates a [[Configuration]] instance for reading application settings from a file.
   * @param configPath the path to the configuration file
   * @tparam F the effect type
   * @return a [[Configuration]] instance
   */
  def make[F[_]: Monad](configPath: File): Configuration[F] =
    new Configuration[F] {

      override def read: EitherT[F, ReadFailure, ApplicationSettings] =
        for {
          root <-
            Try(ConfigFactory.parseFile(configPath).withFallback(ConfigFactory.defaultApplication())).toEither
              .leftMap[ReadFailure](failure => ReadFailures.IOFailure(failure.getMessage))
              .toEitherT[F]
          genus <-
            Try(root.getConfig("genus")).toEither
              .leftMap[ReadFailure](_ => ReadFailures.MissingField("genus"))
              .toEitherT[F]
          result = genus.as[ApplicationSettings]
        } yield result
    }
}
