package co.topl.tetra.it

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

trait IntegrationSuite extends CatsEffectSuite {
  type F[A] = IO[A]

  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def munitTimeout: Duration = 10.minutes

}
