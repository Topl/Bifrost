package co.topl.genus

import cats.effect.IO
import cats.implicits._
import co.topl.typeclasses.implicits._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.TransactionId
import co.topl.genus.services.{TransactionReceipt, _}
import co.topl.genusLibrary.algebras.TransactionFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.models.ModelGenerators.GenHelper
import io.grpc.{Metadata, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GrpcTransactionServiceSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("getTransactionById: Exceptions") {
    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {
        val transactionFetcher = mock[TransactionFetcherAlgebra[F]]
        val underTest = new GrpcTransactionService[F](transactionFetcher)

        (transactionFetcher.fetchTransactionReceipt _)
          .expects(transactionId)
          .once()
          .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[Option[TransactionReceipt]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getTransactionById(GetTransactionByIdRequest(transactionId), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getTransactionById: Not Found") {
    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {
        val transactionFetcher = mock[TransactionFetcherAlgebra[F]]
        val underTest = new GrpcTransactionService[F](transactionFetcher)

        (transactionFetcher.fetchTransactionReceipt _)
          .expects(transactionId)
          .once()
          .returning(Option.empty[TransactionReceipt].asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: TransactionId:${transactionId.show}")(
            underTest.getTransactionById(GetTransactionByIdRequest(transactionId), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getTransactionById: Ok") {
    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {
        val transactionFetcher = mock[TransactionFetcherAlgebra[F]]
        val underTest = new GrpcTransactionService[F](transactionFetcher)

        val ioTransaction = arbitraryIoTransaction.arbitrary.first
        val blockId = arbitraryBlockId.arbitrary.first
        val transactionReceipt = TransactionReceipt(
          ioTransaction,
          ConfidenceFactor.defaultInstance,
          blockId,
          ChainDistance.defaultInstance
        )

        (transactionFetcher.fetchTransactionReceipt _)
          .expects(transactionId)
          .once()
          .returning(transactionReceipt.some.asRight[GE].pure[F])

        for {
          res <- underTest.getTransactionById(GetTransactionByIdRequest(transactionId), new Metadata())
          _ = assert(res.transactionReceipt.blockId == blockId)
        } yield ()
      }
    }

  }

}
