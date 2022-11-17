package co.topl.genusLibrary.interpreter

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.genusLibrary.failure.Failures.{NoBlockBodyFoundOnNodeFailure, NoBlockHeaderFoundOnNodeFailure, NonExistentTransactionsFailure}
import co.topl.models.ModelGenerators._
import co.topl.models._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.ListSet

class NodeBlockFetcherSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  val toplRpc: ToplRpc[F, Any] = mock[ToplRpc[F, Any]]

  val nodeBlockFetcher = new NodeBlockFetcher(toplRpc)

  test("On no block at given height, a None should be returned") {
    PropF.forAllF { height: Long =>
      withMock {

        (toplRpc.blockIdAtHeight _)
          .expects(height)
          .returning(Option.empty[TypedIdentifier].pure[F])
          .once()

        val result = nodeBlockFetcher fetch height

        result map { actualResult =>
          assertEquals(actualResult, None.asRight)
        }

      }
    }
  }

  test(
    "On a block without a header, a Left of NoBlockHeaderFoundOnNodeFailure should be returned"
  ) {
    PropF.forAllF { (height: Long, blockId: TypedIdentifier) =>
      withMock {

        (toplRpc.blockIdAtHeight _)
          .expects(height)
          .returning(blockId.some.pure[F])
          .once()

        (toplRpc.fetchBlockHeader _)
          .expects(blockId)
          .returning(Option.empty[BlockHeaderV2].pure[F])
          .once()

        val result = nodeBlockFetcher fetch height

        result map { actualResult =>
          assertEquals(actualResult, NoBlockHeaderFoundOnNodeFailure(blockId).asLeft)
        }

      }
    }
  }

  test(
    "On a block without a body, a Left of NoBlockBodyFoundOnNodeFailure should be returned"
  ) {
    PropF.forAllF { (height: Long, blockId: TypedIdentifier, blockHeader: BlockHeaderV2) =>
      withMock {

        (toplRpc.blockIdAtHeight _)
          .expects(height)
          .returning(Option(blockId).pure[F])
          .once()

        (toplRpc.fetchBlockHeader _)
          .expects(blockId)
          .returning(Option(blockHeader).pure[F])
          .once()

        (toplRpc.fetchBlockBody _)
          .expects(blockId)
          .returning(Option.empty[BlockBodyV2].pure[F])
          .once()

        val result = nodeBlockFetcher fetch height

        result map { actualResult =>
          assertEquals(actualResult, NoBlockBodyFoundOnNodeFailure(blockId).asLeft)
        }

      }
    }
  }

  test(
    "On a block with a transaction and missing it, " +
    "a Left of NonExistentTransactionsFailure with that txId should be returned"
  ) {
    PropF.forAllF {
      (
        height:        Long,
        blockId:       TypedIdentifier,
        blockHeader:   BlockHeaderV2,
        transactionId: TypedIdentifier
      ) =>
        withMock {

          val blockBody: BlockBodyV2 = ListSet(transactionId)

          (toplRpc.blockIdAtHeight _)
            .expects(height)
            .returning(Option(blockId).pure[F])
            .once()

          (toplRpc.fetchBlockHeader _)
            .expects(blockId)
            .returning(Option(blockHeader).pure[F])
            .once()

          (toplRpc.fetchBlockBody _)
            .expects(blockId)
            .returning(Option(blockBody).pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId)
            .returning(Option.empty[Transaction].pure[F])

          val result = nodeBlockFetcher fetch height

          result map { actualResult =>
            assertEquals(actualResult, NonExistentTransactionsFailure(ListSet(transactionId)).asLeft)
          }

        }
    }
  }

  test(
    "On a block with three transactions and missing two of them, " +
    "a Left of NonExistentTransactionsFailure with the missing txIds should be returned"
  ) {
    PropF.forAllF {
      (
        height: Long,
        blockId: TypedIdentifier,
        blockHeader: BlockHeaderV2,
        transactionId_01: TypedIdentifier,
        transactionId_02: TypedIdentifier,
        transactionId_03: TypedIdentifier,
      ) =>
        withMock {

          val blockBody: BlockBodyV2 = ListSet(
            transactionId_01,
            transactionId_02,
            transactionId_03,
          )

          val transaction_01 = mock[Transaction]

          (toplRpc.blockIdAtHeight _)
            .expects(height)
            .returning(blockId.some.pure[F])
            .once()

          (toplRpc.fetchBlockHeader _)
            .expects(blockId)
            .returning(blockHeader.some.pure[F])
            .once()

          (toplRpc.fetchBlockBody _)
            .expects(blockId)
            .returning(blockBody.some.pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_01)
            .returning(transaction_01.some.pure[F])

          (toplRpc.fetchTransaction _)
            .expects(transactionId_02)
            .returning(Option.empty[Transaction].pure[F])

          (toplRpc.fetchTransaction _)
            .expects(transactionId_03)
            .returning(Option.empty[Transaction].pure[F])

          val result = nodeBlockFetcher fetch height

          result map { actualResult =>
            assertEquals(
              actualResult,
              NonExistentTransactionsFailure(ListSet(transactionId_02, transactionId_03)).asLeft
            )
          }

        }
    }
  }

  test(
    "On a block with three transactions and missing all of them, " +
    "a Left of NonExistentTransactionsFailure with the missing txIds should be returned"
  ) {
    PropF.forAllF {
      (
        height: Long,
        blockId: TypedIdentifier,
        blockHeader: BlockHeaderV2,
        transactionId_01: TypedIdentifier,
        transactionId_02: TypedIdentifier,
        transactionId_03: TypedIdentifier,
      ) =>
        withMock {

          val blockBody: BlockBodyV2 = ListSet(
            transactionId_01,
            transactionId_02,
            transactionId_03,
          )

          (toplRpc.blockIdAtHeight _)
            .expects(height)
            .returning(blockId.some.pure[F])
            .once()

          (toplRpc.fetchBlockHeader _)
            .expects(blockId)
            .returning(blockHeader.some.pure[F])
            .once()

          (toplRpc.fetchBlockBody _)
            .expects(blockId)
            .returning(blockBody.some.pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_01)
            .returning(Option.empty[Transaction].pure[F])

          (toplRpc.fetchTransaction _)
            .expects(transactionId_02)
            .returning(Option.empty[Transaction].pure[F])

          (toplRpc.fetchTransaction _)
            .expects(transactionId_03)
            .returning(Option.empty[Transaction].pure[F])

          val result = nodeBlockFetcher fetch height

          result map { actualResult =>
            assertEquals(
              actualResult,
              NonExistentTransactionsFailure(ListSet(transactionId_01, transactionId_02, transactionId_03)).asLeft
            )
          }

        }
    }
  }

  test(
    "On a block with a header and three transactions, a Right of the full block body should be returned"
  ) {
    PropF.forAllF {
      (
        height: Long,
        blockId: TypedIdentifier,
        blockHeader: BlockHeaderV2,
        transactionId_01: TypedIdentifier,
        transactionId_02: TypedIdentifier,
        transactionId_03: TypedIdentifier,
        transaction_01: Transaction,
        transaction_02: Transaction,
        transaction_03: Transaction,
      ) =>
        withMock {

          val blockBody: BlockBodyV2 = ListSet(
            transactionId_01,
            transactionId_02,
            transactionId_03,
          )

          (toplRpc.blockIdAtHeight _)
            .expects(height)
            .returning(blockId.some.pure[F])
            .once()

          (toplRpc.fetchBlockHeader _)
            .expects(blockId)
            .returning(blockHeader.some.pure[F])
            .once()

          (toplRpc.fetchBlockBody _)
            .expects(blockId)
            .returning(blockBody.some.pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_01)
            .returning(transaction_01.some.pure[F])

          (toplRpc.fetchTransaction _)
            .expects(transactionId_02)
            .returning(transaction_02.some.pure[F])

          (toplRpc.fetchTransaction _)
            .expects(transactionId_03)
            .returning(transaction_03.some.pure[F])

          val result = nodeBlockFetcher fetch height

          result map { actualResult =>
            assertEquals(
              actualResult,
              Option(BlockV2.Full(blockHeader, Chain(transaction_01, transaction_02, transaction_03))).asRight
            )
          }

        }
    }
  }

}
