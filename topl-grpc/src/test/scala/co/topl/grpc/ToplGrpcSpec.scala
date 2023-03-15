package co.topl.grpc

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.brambl.generators.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators._
import co.topl.node.models.BlockBody
import co.topl.node.services._
import fs2.Stream
import io.grpc.Metadata
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class ToplGrpcSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("A transaction can be broadcast") {
    PropF.forAllF { (transaction: IoTransaction) =>
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.broadcastTransaction _)
          .expects(transaction)
          .once()
          .returning(Applicative[F].unit)

        for {
          res <- underTest.broadcastTransaction(BroadcastTransactionReq(transaction), new Metadata())
          _ = assert(res == BroadcastTransactionRes())
        } yield ()
      }
    }
  }

  test("A block header can be retrieved") {
    PropF.forAllF { (header: BlockHeader) =>
      val headerId = header.id
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockHeader _)
          .expects(headerId)
          .once()
          .returning(header.some.pure[F])

        for {
          res <- underTest.fetchBlockHeader(FetchBlockHeaderReq(headerId), new Metadata())
          _ = assert(res.header.get == header)
        } yield ()
      }
    }
  }

  test("A block body can be retrieved") {
    PropF.forAllF { (id: BlockId, body: BlockBody) =>
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockBody _)
          .expects(id)
          .once()
          .returning(body.some.pure[F])

        for {
          res <- underTest.fetchBlockBody(FetchBlockBodyReq(id), new Metadata())

          protoBody = res.body.get
          _ = assert(protoBody == body)
        } yield ()
      }
    }
  }

  test("A transaction can be retrieved") {
    PropF.forAllF { (transaction: IoTransaction) =>
      val transactionId = transaction.id
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchTransaction _)
          .expects(transactionId)
          .once()
          .returning(transaction.some.pure[F])

        for {
          res <- underTest.fetchTransaction(FetchTransactionReq(transactionId), new Metadata())
          _ = assert(transaction == res.transaction.get)
        } yield ()
      }
    }
  }

  test("The block ID at a height can be retrieved") {
    PropF.forAllF { (height: Long, header: BlockHeader) =>
      val blockId = header.id
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.blockIdAtHeight _)
          .expects(height)
          .once()
          .returning(blockId.some.pure[F])

        for {
          res <- underTest.fetchBlockIdAtHeight(FetchBlockIdAtHeightReq(height), new Metadata())
          proto = res.blockId.get
          _ = assert(blockId == proto)
        } yield ()
      }
    }
  }

  // TODO: fetchBlockIdAtDepth has no unit testing
  // TODO: currentMempool has no unit testing
  // TODO: synchronizationTraversal has no unit Testing
}
