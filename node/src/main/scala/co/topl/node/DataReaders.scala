package co.topl.node

import cats.data.{Kleisli, ReaderT}
import cats.effect.{Async, Resource}
import fs2.io.file.{Files, Path}
import fs2.io.net.Network
import org.http4s.ember.client.EmberClientBuilder

object DataReaders {

  type DataReader[F[_]] = ReaderT[F, String, Array[Byte]]

  /**
   * Creates a file reader which loads files within the given local directory
   * @param localDir The local directory containing files
   */
  def fromDisk[F[_]: Async](localDir: Path): DataReader[F] =
    Kleisli(fileName => Files.forAsync[F].readAll(localDir / fileName).compile.to(Array))

  /**
   * Creates a file reader which loads files from a remote URL prefix
   *
   * @param baseUrl The prefix URL containing files.
   *                For example, if files are located at:
   *                http://foo.com/bar/a.txt
   *                http://foo.com/bar/b.txt
   *
   *                Then the baseUrl would be: http://foo.com/bar/
   */
  def fromUrl[F[_]: Async](baseUrl: String): Resource[F, DataReader[F]] = {
    implicit val networkF: Network[F] = Network.forAsync
    EmberClientBuilder
      .default[F]
      .build
      .map(client => Kleisli(fileName => client.expect[Array[Byte]](s"$baseUrl/$fileName")))
  }

}
