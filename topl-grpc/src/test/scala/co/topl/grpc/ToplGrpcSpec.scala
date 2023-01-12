package co.topl.grpc

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.node.services._
import co.topl.proto.models
import co.topl.models.ModelGenerators._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import io.grpc.{Metadata, Status, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import fs2.Stream

class ToplGrpcSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

//  test("A transaction can be broadcast") {
//    PropF.forAllF { (transaction: bifrostModels.Transaction) =>
//      withMock {
//        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
//        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)
//
//        (interpreter.broadcastTransaction _)
//          .expects(transaction)
//          .once()
//          .returning(Applicative[F].unit)
//
//        for {
//          proto <- EitherT(transaction.toF[F, models.Transaction]).getOrElse(???)
//          res   <- underTest.broadcastTransaction(BroadcastTransactionReq(proto.some), new Metadata())
//          _ = assert(res == BroadcastTransactionRes())
//        } yield ()
//      }
//    }
//  }

  test("A block header can be retrieved") {
    PropF.forAllF { (header: co.topl.consensus.models.BlockHeader) =>
      val headerId = header.id.asTypedBytes
      withMock {
        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockHeader _)
          .expects(headerId)
          .once()
          .returning(header.some.pure[F])

        for {
          protoId <- EitherT(headerId.toF[F, models.BlockId]).getOrElse(???)
          res     <- underTest.fetchBlockHeader(FetchBlockHeaderReq(protoId.value), new Metadata())
          protoHeader = res.header.get
//          _header <- EitherT(protoHeader.toF[F, bifrostModels.BlockHeader]).getOrElse(???)
          _ = assert(protoHeader == header)
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
            FetchBlockHeaderReq(models.BlockId(ByteString.EMPTY).value),
            new Metadata()
          )
        )
        _ = assert(e.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
      } yield ()
    }
  }

  // TODO Fix this test, when ToplGrpc is finish method by method: only fetchBlockHeader is done
//  test("A block body can be retrieved") {
//    PropF.forAllF { (_id: bifrostModels.TypedIdentifier, body: bifrostModels.BlockBody) =>
//      val id = bifrostModels.TypedBytes(bifrostModels.IdentifierTypes.Block.HeaderV2, _id.dataBytes)
//      withMock {
//        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
//        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)
//
//        (interpreter.fetchBlockBody _)
//          .expects(id)
//          .once()
//          .returning(body.some.pure[F])
//
//        for {
//          protoId <- EitherT(id.toF[F, models.BlockId]).getOrElse(???)
//          res     <- underTest.fetchBlockBody(FetchBlockBodyReq(protoId.some), new Metadata())
//
//          protoBody = res.body.get
//          _body <- EitherT(protoBody.toF[F, bifrostModels.BlockBody]).getOrElse(???)
//          _ = assert(_body == body)
//        } yield ()
//      }
//    }
//  }
//
//  test("An invalid block body ID is rejected") {
//    withMock {
//      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
//      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)
//
//      for {
//        e <- interceptIO[StatusException](
//          underTest.fetchBlockBody(FetchBlockBodyReq(models.BlockId(ByteString.EMPTY).some), new Metadata())
//        )
//        _ = assert(e.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
//      } yield ()
//    }
//  }
//
//  test("A transaction can be retrieved") {
//    PropF.forAllF { (transaction: bifrostModels.Transaction) =>
//      val transactionId = transaction.id.asTypedBytes
//      withMock {
//        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
//        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)
//
//        (interpreter.fetchTransaction _)
//          .expects(transactionId)
//          .once()
//          .returning(transaction.some.pure[F])
//
//        for {
//          protoId <- EitherT(transactionId.toF[F, models.TransactionId]).getOrElse(???)
//          res     <- underTest.fetchTransaction(FetchTransactionReq(protoId.some), new Metadata())
//
//          proto = res.transaction.get
//          _transaction <- EitherT(proto.toF[F, bifrostModels.Transaction]).getOrElse(???)
//          _ = assert(transaction == _transaction)
//        } yield ()
//      }
//    }
//  }
//
//  test("An invalid transaction ID is rejected") {
//    withMock {
//      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
//      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)
//
//      for {
//        e <- interceptIO[StatusException](
//          underTest.fetchTransaction(FetchTransactionReq(models.TransactionId(ByteString.EMPTY).some), new Metadata())
//        )
//        _ = assert(e.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
//      } yield ()
//    }
//  }
//
//  test("The block ID at a height can be retrieved") {
//    PropF.forAllF { (height: Long, header: bifrostModels.BlockHeader) =>
//      val blockId = header.id.asTypedBytes
//      withMock {
//        val interpreter = mock[ToplRpc[F, Stream[F, *]]]
//        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)
//
//        (interpreter.blockIdAtHeight _)
//          .expects(height)
//          .once()
//          .returning(blockId.some.pure[F])
//
//        for {
//          res <- underTest.fetchBlockIdAtHeight(FetchBlockIdAtHeightReq(height), new Metadata())
//
//          proto = res.blockId.get
//          _id <- EitherT(proto.toF[F, bifrostModels.TypedIdentifier]).getOrElse(???)
//          _ = assert(blockId == _id)
//        } yield ()
//      }
//    }
//  }
}
