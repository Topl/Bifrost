package co.topl.networking.multiplexer

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.std.Queue
import fs2._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import scodec.bits.ByteVector

class MultiplexerSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("accept a subProtocol set") {
    PropF.forAllF(Gen.asciiStr, Gen.asciiStr) { (s1: String, s2: String) =>
      val testResource =
        for {
          inQueue        <- Queue.unbounded[F, Chunk[Byte]].toResource
          outQueue       <- Queue.unbounded[F, Chunk[Byte]].toResource
          handler1Sink   <- Queue.unbounded[F, Chunk[Byte]].toResource
          handler1Source <- Queue.unbounded[F, Chunk[Byte]].toResource
          handler2Sink   <- Queue.unbounded[F, Chunk[Byte]].toResource
          handler2Source <- Queue.unbounded[F, Chunk[Byte]].toResource
          subHandler1 = SubHandler[F](
            1,
            _.enqueueUnterminated(handler1Sink),
            Stream.fromQueueUnterminated(handler1Source)
          )
          subHandler2 = SubHandler[F](
            2,
            _.enqueueUnterminated(handler2Sink),
            Stream.fromQueueUnterminated(handler2Source)
          )
          multiplexerOutcome <-
            Multiplexer(NonEmptyChain(subHandler1, subHandler2))(
              Stream.fromQueueUnterminatedChunk(inQueue),
              _.enqueueUnterminatedChunks(outQueue)
            ).use_.background
          m1 = Chunk.byteVector(ByteVector.encodeAscii(s1).toOption.get)
          m2 = Chunk.byteVector(ByteVector.encodeAscii(s2).toOption.get)
          // Submit message 1 to session 1 and expect it to be forwarded into handler1Sink
          _ <- inQueue.offer(MessageSerializerFramer.function(1, m1)).toResource
          _ <- handler1Sink.take.assertEquals(m1).toResource
          // Have session 1 produce/emit message 2 and expect it to be forwarded to the output queue
          _ <- handler1Source.offer(m2).toResource
          _ <- outQueue.take.flatMap(MessageParserFramer.parseWhole[F]).assertEquals((1: Byte, m2)).toResource
          // Submit message 1 to session 2 and expect it to be forwarded into handler2Sink
          _ <- inQueue.offer(MessageSerializerFramer.function(2, m1)).toResource
          _ <- handler2Sink.take.assertEquals(m1).toResource
          // Have session 2 produce/emit message 2 and expect it to be forwarded to the output queue
          _ <- handler2Source.offer(m2).toResource
          _ <- outQueue.take.flatMap(MessageParserFramer.parseWhole[F]).assertEquals((2: Byte, m2)).toResource
          // Now send to an invalid session and expect an error
          _ <- inQueue.offer(MessageSerializerFramer.function(3, m1)).toResource
          _ <- multiplexerOutcome.map(_.isError).assert.toResource
        } yield ()
      testResource.use_
    }
  }

}
