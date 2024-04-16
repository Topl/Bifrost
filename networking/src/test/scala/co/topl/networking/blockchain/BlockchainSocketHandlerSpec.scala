package co.topl.networking.blockchain

import cats.implicits._
import cats.effect.IO
import cats.effect.std.Queue
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.consensus.models.BlockId
import co.topl.models.Bytes
import co.topl.models.ModelGenerators.GenHelper
import co.topl.networking.multiplexer.MultiplexedReaderWriter
import co.topl.networking.NetworkGen._
import co.topl.models.generators.consensus.ModelGenerators._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import fs2.Stream

import scala.concurrent.duration._

class BlockchainSocketHandlerSpec extends CatsEffectSuite with AsyncMockFactory with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("should provide a BlockchainPeerClient") {
    withMock {
      val testResource =
        for {
          server    <- mock[BlockchainPeerServerAlgebra[F]].pure[F].toResource
          buffers   <- BlockchainMultiplexedBuffers.make[F]
          readQueue <- Queue.unbounded[F, (Int, Bytes)].toResource
          readStream: Stream[F, (Int, Bytes)] = Stream.fromQueueUnterminated(readQueue)
          writer = mockFunction[(Int, Bytes), F[Unit]]
          writeF: Function1[(Int, Bytes), F[Unit]] = writer
          readerWriter = MultiplexedReaderWriter[F](read = readStream, write = (a: Int, b: Bytes) => writeF(a, b))
          cache <- PeerStreamBuffer.make[F]
          connectedPeer = arbitraryConnectedPeer.arbitrary.first
          requestTimeout = 3.seconds

          underTest = new BlockchainSocketHandler[F](
            server,
            buffers,
            readerWriter,
            cache,
            connectedPeer,
            requestTimeout
          )

          _ = (() => server.localBlockAdoptions).expects().once().returning(Stream.never[F].pure[F])
          _ = (() => server.localTransactionNotifications).expects().once().returning(Stream.never[F].pure[F])
          _ = writer.expects((BlockchainMultiplexerId.BlockAdoptionRequest.id, ZeroBS)).once().returning(().pure[F])
          _ = writer
            .expects((BlockchainMultiplexerId.TransactionNotificationRequest.id, ZeroBS))
            .once()
            .returning(().pure[F])

          _ <- underTest.client
            .evalMap(client =>
              for {
                _ <- client.remotePeer.pure[F].assertEquals(connectedPeer)
                blockId = arbitraryBlockId.arbitrary.first
                _ = writer
                  .expects(
                    (
                      BlockchainMultiplexerId.BlockIdAtDepthRequest.id,
                      ZeroBS.concat(Transmittable[Long].transmittableBytes(0L))
                    )
                  )
                  .once()
                  .returning(
                    readQueue.offer(
                      (
                        BlockchainMultiplexerId.BlockIdAtDepthRequest.id,
                        OneBS.concat(Transmittable[Option[BlockId]].transmittableBytes(blockId.some))
                      )
                    )
                  )
                _ <- client.getRemoteBlockIdAtDepth(0L).assertEquals(blockId.some)
              } yield ()
            )
            .compile
            .drain
            .toResource
        } yield ()

      testResource.use_
    }
  }
}
