package co.topl.genus.settings

import cats.data.EitherT

/**
 * Represents a stored application configuration.
 * @tparam F
 */
trait Configuration[F[_]] {

  /**
   * Reads the application settings from a stored config.
   * @return the application settings or a read failure
   */
  def read: EitherT[F, Configuration.ReadFailure, ApplicationSettings]
}

object Configuration {
  sealed trait ReadFailure

  object ReadFailures {
    case class IOFailure(path: String) extends ReadFailure
    case class MissingField(field: String) extends ReadFailure
  }
}
