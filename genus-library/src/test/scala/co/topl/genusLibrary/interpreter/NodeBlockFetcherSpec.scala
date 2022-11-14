package co.topl.genusLibrary.interpreter

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.genusLibrary.failure.Failures.{
  NoBlockBodyFoundOnNodeFailure,
  NoBlockHeaderFoundOnNodeFailure,
  NonExistentTransactionsFailure
}
import co.topl.genusLibrary.interpreter.NodeBlockFetcherSpec.{
  BLOCK_ID,
  HEIGHT,
  TRANSACTION_ID_01,
  TRANSACTION_ID_02,
  TRANSACTION_ID_03
}
import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2, Bytes, Transaction, TypedBytes, TypedIdentifier}
import munit.FunSuite
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.ListSet

class NodeBlockFetcherSpec extends FunSuite with AsyncMockFactory {

  type F[A] = IO[A]

  val toplRpc: ToplRpc[F, Any] = mock[ToplRpc[F, Any]]

  val nodeBlockFetcher = new NodeBlockFetcher(toplRpc)

  test("On no block at given height, a None should be returned") {
    withMock {

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option.empty[TypedIdentifier].pure[F])
        .once()

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(actualResult, None.asRight)
      }

    }
  }

  test(
    "On a block without a header, a Left of NoBlockHeaderFoundOnNodeFailure should be returned"
  ) {
    withMock {

      val blockBody: BlockBodyV2 = ListSet.empty

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option.empty[BlockHeaderV2].pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option(blockBody).pure[F])
        .once()

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(actualResult, NoBlockHeaderFoundOnNodeFailure(BLOCK_ID).asLeft)
      }

    }
  }

  test(
    "On a block without a body, a Left of NoBlockBodyFoundOnNodeFailure should be returned"
  ) {
    withMock {

      val blockHeader = mock[BlockHeaderV2]

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option(blockHeader).pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option.empty[BlockBodyV2].pure[F])
        .once()

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(actualResult, NoBlockBodyFoundOnNodeFailure(BLOCK_ID).asLeft)
      }

    }
  }

  test(
    "On a block without header and body, a Left of NoBlockHeaderFoundOnNodeFailure should be returned"
  ) {
    withMock {

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option.empty[BlockHeaderV2].pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option.empty[BlockBodyV2].pure[F])
        .once()

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(actualResult, NoBlockHeaderFoundOnNodeFailure(BLOCK_ID).asLeft)
      }

    }
  }

  test(
    "On a block with a transaction and missing it, " +
    "a Left of NonExistentTransactionsFailure with that txId should be returned"
  ) {
    withMock {

      val blockHeader = mock[BlockHeaderV2]

      val blockBody: BlockBodyV2 = ListSet(
        TRANSACTION_ID_01
      )

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option(blockHeader).pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option(blockBody).pure[F])
        .once()

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_01)
        .returning(Option.empty[Transaction].pure[F])

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(actualResult, NonExistentTransactionsFailure(ListSet(TRANSACTION_ID_01)).asLeft)
      }

    }
  }

  test(
    "On a block with three transactions and missing two of them, " +
    "a Left of NonExistentTransactionsFailure with the missing txIds should be returned"
  ) {
    withMock {

      val blockHeader = mock[BlockHeaderV2]

      val blockBody: BlockBodyV2 = ListSet(
        TRANSACTION_ID_01,
        TRANSACTION_ID_02,
        TRANSACTION_ID_03
      )

      val transaction_01 = mock[Transaction]

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option(blockHeader).pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option(blockBody).pure[F])
        .once()

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_01)
        .returning(Option(transaction_01).pure[F])

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_02)
        .returning(Option.empty[Transaction].pure[F])

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_03)
        .returning(Option.empty[Transaction].pure[F])

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(
          actualResult,
          NonExistentTransactionsFailure(ListSet(TRANSACTION_ID_02, TRANSACTION_ID_03)).asLeft
        )
      }

    }
  }

  test(
    "On a block with three transactions and missing all of them, " +
    "a Left of NonExistentTransactionsFailure with the missing txIds should be returned"
  ) {
    withMock {

      val blockHeader = mock[BlockHeaderV2]

      val blockBody: BlockBodyV2 = ListSet(
        TRANSACTION_ID_01,
        TRANSACTION_ID_02,
        TRANSACTION_ID_03
      )

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option(blockHeader).pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option(blockBody).pure[F])
        .once()

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_01)
        .returning(Option.empty[Transaction].pure[F])

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_02)
        .returning(Option.empty[Transaction].pure[F])

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_03)
        .returning(Option.empty[Transaction].pure[F])

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(
          actualResult,
          NonExistentTransactionsFailure(ListSet(TRANSACTION_ID_01, TRANSACTION_ID_02, TRANSACTION_ID_03)).asLeft
        )
      }

    }
  }

  test(
    "On a block with a header and three transactions, a Right of the full block body should be returned"
  ) {
    withMock {

      val blockHeader = mock[BlockHeaderV2]

      val blockBody: BlockBodyV2 = ListSet(
        TRANSACTION_ID_01,
        TRANSACTION_ID_02,
        TRANSACTION_ID_03
      )

      val transaction_01 = mock[Transaction]
      val transaction_02 = mock[Transaction]
      val transaction_03 = mock[Transaction]

      (toplRpc.blockIdAtHeight _)
        .expects(HEIGHT)
        .returning(Option(BLOCK_ID).pure[F])
        .once()

      (toplRpc.fetchBlockHeader _)
        .expects(BLOCK_ID)
        .returning(Option(blockHeader).pure[F])
        .once()

      (toplRpc.fetchBlockBody _)
        .expects(BLOCK_ID)
        .returning(Option(blockBody).pure[F])
        .once()

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_01)
        .returning(Option(transaction_01).pure[F])

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_02)
        .returning(Option(transaction_02).pure[F])

      (toplRpc.fetchTransaction _)
        .expects(TRANSACTION_ID_03)
        .returning(Option(transaction_03).pure[F])

      val result = nodeBlockFetcher fetch HEIGHT

      result map { actualResult =>
        assertEquals(
          actualResult,
          Option(BlockV2.Full(blockHeader, Chain(transaction_01, transaction_02, transaction_03))).asRight
        )
      }

    }
  }

}

object NodeBlockFetcherSpec {

  final private val HEIGHT: Long = 1

  final private val BLOCK_ID: TypedIdentifier = TypedBytes(-1: Byte, Bytes(-1: Byte))

  final private val TRANSACTION_ID_01: TypedIdentifier = TypedBytes(-2: Byte, Bytes(-2: Byte))
  final private val TRANSACTION_ID_02: TypedIdentifier = TypedBytes(-3: Byte, Bytes(-3: Byte))
  final private val TRANSACTION_ID_03: TypedIdentifier = TypedBytes(-4: Byte, Bytes(-4: Byte))

}
