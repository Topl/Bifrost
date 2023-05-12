package co.topl.networking.p2p

import cats.MonadThrow
import cats.effect.Async
import cats.implicits._
import cats.effect.std.Random
import co.topl.crypto.hash.Blake2b256
import fs2.Chunk

import java.nio.ByteBuffer

/**
 * Describes the leader of a connection between two peers.  A leader doesn't have any special authority; it is
 * primarily meant as a way of handling a coin-flip decision between the two peers.
 */
sealed abstract class ConnectionLeader

object ConnectionLeader {
  case object Local extends ConnectionLeader
  case object Remote extends ConnectionLeader

  /**
   * Establish a Connection Leader for the given reader/writer functions.
   * Each peer generates a random number locally.  The number is then hashed and the hash is sent to the remote peer.
   * Once received, the peers then exchange the original numbers for verification.  Finally, the two numbers are run
   * through a function which deterministically outputs the leader.
   * @param readN Reads N bytes from the socket
   * @param write Writes bytes to the socket
   * @return the leader of the socket
   */
  def fromSocket[F[_]: Async: Random](
    readN: Int => F[Chunk[Byte]],
    write: Chunk[Byte] => F[Unit]
  ): F[ConnectionLeader] =
    for {
      localValue <- Random[F].nextInt
      localValueBytes = intToBytestring(localValue)
      blake2b256 = new Blake2b256()
      localValueDigest = blake2b256.hash(localValueBytes)
      _                <- write(Chunk.array(localValueDigest))
      remoteDigest     <- readN(32).map(_.toArray)
      _                <- write(Chunk.array(localValueBytes))
      remoteValueBytes <- readN(4).map(_.toArray)
      _ <- MonadThrow[F].raiseWhen(
        !java.util.Arrays.equals(remoteDigest, blake2b256.hash(remoteValueBytes))
      )(new IllegalStateException("Remote evidence did not match remote value"))
      connectionLeader =
        if (
          BigInt(blake2b256.hash(localValueBytes ++ remoteValueBytes)) >
          BigInt(blake2b256.hash(remoteValueBytes ++ localValueBytes))
        )
          ConnectionLeader.Local
        else ConnectionLeader.Remote
    } yield connectionLeader

  private def intToBytestring(value: Int): Array[Byte] =
    ByteBuffer.allocate(4).putInt(value).array()
}
