package co.topl.node

import cats.implicits._
import cats.data.{Kleisli, ReaderT}
import cats.effect.{Async, Resource}
import fs2.io.file.{Files, Path}
import org.http4s.ember.client.EmberClientBuilder


object BigBangReaders {
  def fromDisk[F[_]: Async](localDir: Path): ReaderT[F, String, Array[Byte]] =
    Kleisli(fileName => Files[F].readAll(localDir / fileName).compile.to(Array))

  def fromUrl[F[_]: Async](baseUrl: String): Resource[F, ReaderT[F, String, Array[Byte]]] =
    EmberClientBuilder.default[F].build.map(client =>
      Kleisli(fileName => client.expect[Array[Byte]](s"$baseUrl/$fileName"))
    )

}
