package co.topl.genus.settings

import cats.data.EitherT
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.genus.settings.Configuration.{ReadFailure, ReadFailures}
import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import java.io.File

object FileConfiguration {

  /**
   * Creates a [[Configuration]] instance for reading application settings from a file.
   * @param configPath the path to the configuration file
   * @tparam F the effect type
   * @return a [[Configuration]] instance
   */
  def make[F[_]: Sync](configPath: File): Configuration[F] =
    new Configuration[F] {

      override def read: EitherT[F, ReadFailure, ApplicationSettings] =
        for {
          root <-
            EitherT(
              Sync[F]
                .blocking(
                  ConfigFactory
                    .parseFile(configPath)
                    .withFallback(ConfigFactory.defaultApplication())
                )
                .map(_.asRight[ReadFailure])
                .handleError(failure => ReadFailures.IOFailure(failure.getMessage).asLeft)
            )
          genus <-
            Sync[F]
              .attemptT(root.getConfig("genus").pure[F])
              .leftMap[ReadFailure](_ => ReadFailures.MissingField("genus"))
          result = genus.as[ApplicationSettings]
        } yield result
    }
}
