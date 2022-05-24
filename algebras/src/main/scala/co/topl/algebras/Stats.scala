package co.topl.algebras

import io.circe.Json

trait Stats[F[_]] {
  def write(statName: String, data: Json): F[Unit]
}
