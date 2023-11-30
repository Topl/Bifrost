package co.topl.version

import cats.data.OptionT
import cats.effect.implicits.genSpawnOps
import cats.implicits._
import cats.effect.kernel.{Async, Outcome, Resource}
import co.topl.algebras.SoftwareVersionAlgebra
import co.topl.blockchain.algebras.NodeMetadataAlgebra
import org.http4s.ember.client.EmberClientBuilder
import fs2.Stream
import org.typelevel.log4cats.Logger
import scala.concurrent.duration._
import io.circe.parser._

object VersionReplicator {

  def make[F[_]: Async](
    nodeMetadataAlgebra: NodeMetadataAlgebra[F],
    uri:                 String
  ): Resource[F, SoftwareVersionAlgebra[F]] =
    Resource.pure {
      new SoftwareVersionAlgebra[F] {
        override def fetchSoftwareVersion(): F[String] =
          OptionT(nodeMetadataAlgebra.readAppVersion).getOrElse("Undefined")

        override def fetchLatestSoftwareVersion(): F[String] =
          EmberClientBuilder
            .default[F]
            .build
            .map(_.expect[String](uri))
            .useEval
            .map(s =>
              parse(s)
                .leftMap(_ => "Undefined")
                .map(_.\\("tag_name").headOption.map(_.noSpaces).getOrElse("Undefined"))
                .merge
            )
      }
    }

  def background[F[_]: Async: Logger](
    interpreter:  SoftwareVersionAlgebra[F],
    rateInterval: FiniteDuration
  ): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    Stream
      .repeatEval(
        for {
          sv  <- interpreter.fetchSoftwareVersion()
          lsv <- interpreter.fetchLatestSoftwareVersion()

          /**
           * current log: INFO  Bifrost.Node - Software Version:[2.0.0-alpha10-59-49f994b7-20231129-1055] - Latest:[Some("v2.0.0-alpha10")]
           * We could raise a warning when current < latest, (patch, minor or mayor), do it later when alpha, beta pattern is deprecated
           *  - 2.0.0 < "v2.0.1"
           *  - 2.0.0 < "v2.1.0"
           *  - 2.0.0 < "v3.0.0"
           */
          res <- Logger[F].info(s"Software Version:[$sv] - Latest:[$lsv]")
        } yield res
      )
      .metered(rateInterval)
      .compile
      .drain
      .background

}
