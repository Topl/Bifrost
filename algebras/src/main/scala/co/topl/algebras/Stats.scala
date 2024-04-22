package co.topl.algebras

import io.circe.Json
import cats.Applicative

trait Stats[F[_]] {
  def incrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit]
  def decrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit]
  def recordGauge(statName:      String, description: String, attributes: Map[String, Json], value: Json): F[Unit]
  def recordHistogram(statName:  String, description: String, attributes: Map[String, Json], value: Json): F[Unit]
  def writeFile(statName: String, data: Json): F[Unit]
}

object Stats {
  def apply[F[_]](implicit F: Stats[F]): Stats[F] = F

  def noop[F[_]: Applicative]: Stats[F] = new Stats[F] {

    def incrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit] =
      Applicative[F].unit

    def decrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit] =
      Applicative[F].unit

    def recordGauge(statName: String, description: String, attributes: Map[String, Json], value: Json): F[Unit] =
      Applicative[F].unit

    def recordHistogram(statName: String, description: String, attributes: Map[String, Json], value: Json): F[Unit] =
      Applicative[F].unit

    def writeFile(statName: String, data: Json): F[Unit] =
      Applicative[F].unit
  }

  object Implicits {
    implicit def noop[F[_]: Applicative]: Stats[F] = Stats.noop
  }
}
