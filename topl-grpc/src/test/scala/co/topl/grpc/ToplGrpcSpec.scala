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
import co.topl.models.utility._
import legacyModels.ModelGenerators._
import legacyModels.{IdentifierTypes, TypedBytes, TypedIdentifier}
import co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody
import co.topl.models.generators.models.ModelGenerators.arbitraryTransaction
import co.topl.models.generators.consensus.ModelGenerators.arbitraryHeader
import co.topl.typeclasses.implicits._
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
          res <- underTest.broadcastTransaction(BroadcastTransactionReq(transaction), new Metadata())
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
          res     <- underTest.fetchBlockHeader(FetchBlockHeaderReq(protoId), new Metadata())
          _ = assert(res.header.get == header)
        } yield ()
      }
    }
  }

  /**
   * TODO, ask how to handle this situation,
   * scalapb.validate.FieldValidationException: Validation failed: BlockId.value: length must be 32 - Got []
   */
  test("An invalid block header ID is rejected".fail) {
    withMock {
      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[StatusException](
          underTest.fetchBlockHeader(
            FetchBlockHeaderReq(BlockId.defaultInstance),
            new Metadata()
          )
        )
        _ = assert(e.getStatus.getCode == Status.Code.UNKNOWN)
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
          res     <- underTest.fetchBlockBody(FetchBlockBodyReq(protoId), new Metadata())

          protoBody = res.body.get
          _ = assert(protoBody == body)
        } yield ()
      }
    }
  }

  /**
   * TODO, ask how to handle this situation,
   * scalapb.validate.FieldValidationException: Validation failed: BlockId.value: length must be 32 - Got []
   */
  test("An invalid block body ID is rejected".fail) {
    withMock {
      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[StatusException](
          underTest.fetchBlockBody(FetchBlockBodyReq(BlockId.defaultInstance), new Metadata())
        )
        _ = assert(e.getStatus.getCode == Status.Code.UNKNOWN)
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
          res    <- underTest.fetchTransaction(FetchTransactionReq(ioTx32), new Metadata())
          _ = assert(transaction == res.transaction.get)
        } yield ()
      }
    }
  }

  /**
   * Related to failed 2 cases, TODO, in this case works, why?
   */
  test("An invalid transaction ID is rejected") {
    withMock {
      val interpreter = mock[ToplRpc[F, Stream[F, *]]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[StatusException](
          underTest.fetchTransaction(
            FetchTransactionReq(IoTransaction32.defaultInstance),
            new Metadata()
          )
        )
        _ = assert(e.getStatus.getCode == Status.Code.UNKNOWN)
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
          proto = res.blockId.get: TypedIdentifier
          _ = assert(blockId == proto)
        } yield ()
      }
    }
  }

  // TODO: fetchBlockIdAtDepth has no unit testing
  // TODO: currentMempool has no unit testing
  // TODO: synchronizationTraversal has no unit Testing
}
