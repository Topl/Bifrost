package co.topl.networking.p2p

import cats.MonadThrow
import cats.effect.Async
import cats.implicits._
import cats.effect.std.Random
import co.topl.crypto.hash.Blake2b256
import fs2.Chunk
import fs2.io.net.Socket

import java.nio.ByteBuffer

sealed abstract class SocketLeader

object SocketLeader {
  case object Local extends SocketLeader
  case object Remote extends SocketLeader

  def fromSocket[F[_]: Async: Random](socket: Socket[F]): F[SocketLeader] =
    for {
      localValue <- Random[F].nextInt
      localValueBytes = intToBytestring(localValue)
      blake2b256 = new Blake2b256()
      localValueDigest = blake2b256.hash(localValueBytes)
      _                <- socket.write(Chunk.array(localValueDigest))
      remoteDigest     <- socket.readN(32).map(_.toArray)
      _                <- socket.write(Chunk.array(localValueBytes))
      remoteValueBytes <- socket.readN(4).map(_.toArray)
      _ <- MonadThrow[F].raiseWhen(
        !java.util.Arrays.equals(remoteDigest, blake2b256.hash(remoteValueBytes))
      )(new IllegalStateException("Remote evidence did not match remote value"))
      connectionLeader =
        if (
          BigInt(blake2b256.hash(localValueBytes ++ remoteValueBytes)) >
          BigInt(blake2b256.hash(remoteValueBytes ++ localValueBytes))
        )
          SocketLeader.Local
        else SocketLeader.Remote
    } yield connectionLeader

  private def intToBytestring(value: Int): Array[Byte] =
    ByteBuffer.allocate(4).putInt(value).array()

  private def bytestringToInt(bytes: Array[Byte]): Int =
    ByteBuffer.wrap(bytes).getInt
}
