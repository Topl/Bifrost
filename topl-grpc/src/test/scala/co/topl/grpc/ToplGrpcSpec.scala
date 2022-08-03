package co.topl.grpc

import akka.grpc.GrpcServiceException
import cats.Applicative
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.grpc.services.{BroadcastTransactionReq, BroadcastTransactionRes, FetchBlockHeaderReq}
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockHeaderV2, Bytes, Transaction}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import io.grpc.Status

class ToplGrpcSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("A transaction can be broadcast") {
    PropF.forAllF { (transaction: Transaction) =>
      withMock {
        val interpreter = mock[ToplRpc[F]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.broadcastTransaction _)
          .expects(transaction)
          .once()
          .returning(Applicative[F].unit)

        Async[F]
          .fromFuture(
            underTest.broadcastTransaction(BroadcastTransactionReq(transaction.immutableBytes)).pure[F]
          )
          .assertEquals(BroadcastTransactionRes())
      }
    }
  }

  test("A block header can be retrieved") {
    PropF.forAllF { (header: BlockHeaderV2) =>
      val headerId = header.id.asTypedBytes
      withMock {
        val interpreter = mock[ToplRpc[F]]
        val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

        (interpreter.fetchBlockHeader _)
          .expects(headerId)
          .once()
          .returning(header.some.pure[F])

        for {
          res <- Async[F]
            .fromFuture(
              underTest.fetchBlockHeader(FetchBlockHeaderReq(headerId.transmittableBytes)).pure[F]
            )
          protoHeader = res.header.get
          _ = assert((protoHeader.parentHeaderId: Bytes) == header.parentHeaderId.transmittableBytes)
          _ = assert(protoHeader.parentSlot == header.parentSlot)
          _ = assert((protoHeader.txRoot: Bytes) == header.txRoot.data)
          _ = assert((protoHeader.bloomFilter: Bytes) == header.bloomFilter.data)
          _ = assert(protoHeader.timestamp == header.timestamp)
          _ = assert(protoHeader.height == header.height)
          _ = assert(protoHeader.slot == header.slot)
          _ = assert((protoHeader.eligibilityCertificate: Bytes) == header.eligibilityCertificate.transmittableBytes)
          _ = assert((protoHeader.operationalCertificate: Bytes) == header.operationalCertificate.transmittableBytes)
          _ =
            assert(
              protoHeader.metadata.map(_.value.toByteArray).map(Bytes(_)) == header.metadata
                .map(_.data.bytes)
                .map(Bytes(_))
            )
          _ = assert((protoHeader.address: Bytes) == header.address.transmittableBytes)
        } yield ()
      }
    }
  }

  test("An invalid block header ID is rejected") {
    withMock {
      val interpreter = mock[ToplRpc[F]]
      val underTest = new ToplGrpc.Server.GrpcServerImpl[F](interpreter)

      for {
        e <- interceptIO[GrpcServiceException](
          Async[F]
            .fromFuture(
              underTest.fetchBlockHeader(FetchBlockHeaderReq(ByteString.EMPTY)).pure[F]
            )
        )
        _ = assert(e.status.getCode == Status.Code.INVALID_ARGUMENT)
      } yield ()
    }
  }
}
