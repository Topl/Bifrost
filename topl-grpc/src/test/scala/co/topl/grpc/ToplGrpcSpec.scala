package co.topl.grpc

import cats.Applicative
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.node.services._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody
import co.topl.proto.models.Transaction
import co.topl.{models => legacyModels}
import legacyModels.ModelGenerators._
import legacyModels.{IdentifierTypes, TypedBytes, TypedIdentifier}
import co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody
import co.topl.models.generators.models.ModelGenerators.arbitraryTransaction
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import io.grpc.{Metadata, Status, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import fs2.Stream

class ToplGrpcSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("A transaction can be broadcast") {
    PropF.forAllF { (transaction: Transaction) =>
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.broadcastTransaction _)
          .expects(transaction)
          .once()
          .returning(Applicative[F].unit)

        for {
          res <- underTest.broadcastTransaction(BroadcastTransactionReq(transaction.some), new Metadata())
          _ = assert(res == BroadcastTransactionRes())
        } yield ()
      }
    }
  }

  test("A block header can be retrieved") {
    PropF.forAllF { (header: BlockHeader) =>
      val headerId = header.id.asTypedBytes
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockHeader _)
          .expects(headerId)
          .once()
          .returning(header.some.pure[F])

        for {
          protoId <- EitherT(headerId.toF[F, BlockId]).getOrElse(???)
          res     <- underTest.fetchBlockHeader(FetchBlockHeaderReq(protoId.some), new Metadata())
          _ = assert(res.header.get == header)
        } yield ()
      }
    }
  }

  test("An invalid block header ID is rejected") {
    withMock {
      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[StatusException](
          underTest.fetchBlockHeader(
            FetchBlockHeaderReq(BlockId(ByteString.EMPTY).some),
            new Metadata()
          )
        )
        _ = assert(e.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
      } yield ()
    }
  }

  test("A block body can be retrieved") {
    PropF.forAllF { (_id: TypedIdentifier, body: BlockBody) =>
      val id = TypedBytes(IdentifierTypes.Block.HeaderV2, _id.dataBytes)
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockBody _)
          .expects(id)
          .once()
          .returning(body.some.pure[F])

        for {
          protoId <- EitherT(id.toF[F, BlockId]).getOrElse(???)
          res     <- underTest.fetchBlockBody(FetchBlockBodyReq(protoId.some), new Metadata())

          protoBody = res.body.get
          _ = assert(protoBody == body)
        } yield ()
      }
    }
  }

  test("An invalid block body ID is rejected") {
    withMock {
      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[StatusException](
          underTest.fetchBlockBody(FetchBlockBodyReq(None), new Metadata())
        )
        _ = assert(e.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
      } yield ()
    }
  }

  test("A transaction can be retrieved") {
    PropF.forAllF { (transaction: Transaction) =>
      val transactionId = transaction.id.asTypedBytes
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchTransaction _)
          .expects(transactionId)
          .once()
          .returning(transaction.some.pure[F])

        for {

          ioTx32 <- EitherT(transactionId.toF[F, IoTransaction32]).getOrElse(???)
          res    <- underTest.fetchTransaction(FetchTransactionReq(ioTx32.some), new Metadata())
          _ = assert(transaction == res.transaction.get)
        } yield ()
      }
    }
  }

  test("An invalid transaction ID is rejected") {
    withMock {
      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[StatusException](
          underTest.fetchTransaction(
            FetchTransactionReq(IoTransaction32.of(None).some),
            new Metadata()
          )
        )
        _ = assert(e.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
      } yield ()
    }
  }

  test("The block ID at a height can be retrieved") {
    PropF.forAllF { (height: Long, header: BlockHeader) =>
      val blockId = header.id.asTypedBytes
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.blockIdAtHeight _)
          .expects(height)
          .once()
          .returning(blockId.some.pure[F])

        for {
          res <- underTest.fetchBlockIdAtHeight(FetchBlockIdAtHeightReq(height), new Metadata())
          proto = TypedBytes.headerFromBlockId(res.blockId)
          _ = assert(blockId == proto)
        } yield ()
      }
    }
  }

  // TODO: fetchBlockIdAtDepth has no unit testing
  // TODO: currentMempool has no unit testing
}
