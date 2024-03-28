package co.topl.genus

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.models.{LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.genus.algebras.TransactionFetcherAlgebra
import co.topl.genus.model.{GE, GEs}
import co.topl.genus.services._
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.typeclasses.implicits._
import io.grpc.{Metadata, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GrpcTransactionServiceTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
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

  test("getTxosByLockAddress: Exceptions") {
    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {
        val transactionFetcher = mock[TransactionFetcherAlgebra[F]]
        val underTest = new GrpcTransactionService[F](transactionFetcher)

        (transactionFetcher.fetchTransactionByLockAddress _)
          .expects(lockAddress, TxoState.SPENT)
          .once()
          .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[List[Txo]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getTxosByLockAddress(QueryByLockAddressRequest(lockAddress), new Metadata())
          )
        } yield ()
      }
    }
  }

  test("getTxosByLockAddress: Empty sequence") {
    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {
        val transactionFetcher = mock[TransactionFetcherAlgebra[F]]
        val underTest = new GrpcTransactionService[F](transactionFetcher)

        (transactionFetcher.fetchTransactionByLockAddress _)
          .expects(lockAddress, TxoState.SPENT)
          .once()
          .returning(List.empty[Txo].asRight[GE].pure[F])

        for {
          res <- underTest.getTxosByLockAddress(QueryByLockAddressRequest(lockAddress), new Metadata())
          _ = assert(res.txos.isEmpty)

        } yield ()
      }
    }
  }

  test("getTxosByLockAddress: ok") {
    PropF.forAllF {
      (
        lockAddress:       LockAddress,
        transactionOutput: UnspentTransactionOutput,
        outputAddress:     TransactionOutputAddress
      ) =>
        withMock {
          val transactionFetcher = mock[TransactionFetcherAlgebra[F]]
          val underTest = new GrpcTransactionService[F](transactionFetcher)
          val txo = Txo(
            transactionOutput,
            state = TxoState.SPENT,
            outputAddress
          )

          (transactionFetcher.fetchTransactionByLockAddress _)
            .expects(lockAddress, TxoState.SPENT)
            .once()
            .returning(List(txo).asRight[GE].pure[F])

          for {
            res <- underTest.getTxosByLockAddress(QueryByLockAddressRequest(lockAddress), new Metadata())
            _ = assert(res.txos.head == txo)

          } yield ()
        }
    }
  }
}
