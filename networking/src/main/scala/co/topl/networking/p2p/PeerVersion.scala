package co.topl.networking.p2p

import cats.data.{EitherT, OptionT}
import cats.effect.Async
import cats.implicits._
import co.topl.networking._
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.net.Socket

object PeerVersion {

  /**
   * Exchanges the local network protocol version with the remote peer
   *
   * @param localVersion An array of bytes indicating the version of this local node's network layer
   * @return The remote peer's version
   */
  def extractor[F[_]: Async](
    localVersion: ByteString
  )(socket: Socket[F]): F[Either[ExtractionException, ByteString]] =
    (
      for {
        _ <- EitherT.liftF(socket.write(Chunk.byteBuffer(localVersion.asReadOnlyByteBuffer())))
        remoteVersion <- OptionT(socket.readExactly(localVersion.size()))
          .toRight(ExtractionException.VersionNotProvided)
          .leftWiden[ExtractionException]
          .map(_.toArray)
        remoteVersionBS = ByteString.copyFrom(remoteVersion)
      } yield remoteVersionBS
    ).value

  sealed abstract class ExtractionException extends Exception

  object ExtractionException {
    case object VersionNotProvided extends ExtractionException
  }

}
