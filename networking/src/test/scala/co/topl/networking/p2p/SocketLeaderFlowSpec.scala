package co.topl.networking.p2p

import cats.effect.IO
import cats.effect.std.Random
import cats.implicits._
import co.topl.crypto.hash.Blake2b256
import fs2.Chunk
import munit.CatsEffectSuite
import org.scalamock.munit.AsyncMockFactory

import java.nio.ByteBuffer

class SocketLeaderFlowSpec extends CatsEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test(
    "establish a connection leader as local if " +
    "BigInt(localNumberBytes ++ remoteNumberBytes) > BigInt(remoteNumberBytes ++ localNumberBytes)"
  ) {
    withMock {

      val random2 = new scala.util.Random(0)
      val expectedPublishedInt = random2.nextInt()
      val expectedPublishedEvidence =
        new Blake2b256().hash(intToBytestring(expectedPublishedInt).toArray)

      val remoteInt = expectedPublishedInt + 1
      val remoteIntEvidence = new Blake2b256().hash(intToBytestring(remoteInt).toArray)

      val expectedConnectionLeader =
        if (
          BigInt(
            new Blake2b256()
              .hash(intToBytestring(expectedPublishedInt).toArray ++ intToBytestring(remoteInt).toArray)
          ) >
          BigInt(
            new Blake2b256()
              .hash(intToBytestring(remoteInt).toArray ++ intToBytestring(expectedPublishedInt).toArray)
          )
        ) SocketLeader.Local
        else SocketLeader.Remote

      val reader = mockFunction[Int, F[Chunk[Byte]]]
      val writer = mockFunction[Chunk[Byte], F[Unit]]

      reader
        .expects(32)
        .once()
        .returning(Chunk.array(remoteIntEvidence).pure[F])

      reader
        .expects(4)
        .once()
        .returning(intToBytestring(remoteInt).pure[F])

      writer
        .expects(Chunk.array(expectedPublishedEvidence))
        .once()
        .returning(().pure[F])

      writer
        .expects(intToBytestring(expectedPublishedInt))
        .once()
        .returning(().pure[F])

      for {
        implicit0(random1: Random[F]) <- Random.scalaUtilRandomSeedInt[F](0)
        _                             <- SocketLeader.fromSocket(reader, writer).assertEquals(expectedConnectionLeader)
      } yield ()
    }
  }

  private def intToBytestring(i: Int): Chunk[Byte] =
    Chunk.array(
      ByteBuffer.allocate(4).putInt(i).array()
    )

}
