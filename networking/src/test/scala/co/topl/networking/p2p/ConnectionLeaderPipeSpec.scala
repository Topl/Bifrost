package co.topl.networking.p2p

import cats.effect.IO
import cats.effect.std.Random
import cats.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.networking._
import fs2.Chunk
import munit.CatsEffectSuite
import org.scalamock.munit.AsyncMockFactory

class ConnectionLeaderPipeSpec extends CatsEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test(
    "establish a connection leader as local if " +
    "BigInt(localNumberBytes ++ remoteNumberBytes) > BigInt(remoteNumberBytes ++ localNumberBytes)"
  ) {
    withMock {

      val random2 = new scala.util.Random(0)
      val expectedPublishedInt = random2.nextInt()
      val expectedPublishedEvidence =
        new Blake2b256().hash(encodeInt(expectedPublishedInt))

      val remoteInt = expectedPublishedInt + 1
      val remoteIntEvidence = new Blake2b256().hash(encodeInt(remoteInt))

      val expectedConnectionLeader =
        if (
          BigInt(
            new Blake2b256()
              .hash(encodeInt(expectedPublishedInt) ++ encodeInt(remoteInt))
          ) >
          BigInt(
            new Blake2b256()
              .hash(encodeInt(remoteInt) ++ encodeInt(expectedPublishedInt))
          )
        ) ConnectionLeader.Local
        else ConnectionLeader.Remote

      val reader = mockFunction[Int, F[Chunk[Byte]]]
      val writer = mockFunction[Chunk[Byte], F[Unit]]

      reader
        .expects(32)
        .once()
        .returning(Chunk.array(remoteIntEvidence).pure[F])

      reader
        .expects(4)
        .once()
        .returning(Chunk.array(encodeInt(remoteInt)).pure[F])

      writer
        .expects(Chunk.array(expectedPublishedEvidence))
        .once()
        .returning(().pure[F])

      writer
        .expects(Chunk.array(encodeInt(expectedPublishedInt)))
        .once()
        .returning(().pure[F])

      for {
        implicit0(random1: Random[F]) <- Random.scalaUtilRandomSeedInt[F](0)
        _ <- ConnectionLeader.fromSocket(reader, writer).assertEquals(expectedConnectionLeader)
      } yield ()
    }
  }

}
