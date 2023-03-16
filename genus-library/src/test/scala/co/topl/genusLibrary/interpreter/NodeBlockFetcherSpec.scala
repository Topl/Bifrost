package co.topl.genusLibrary.interpreter

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.failure.Failures.NoBlockBodyFoundOnNodeFailure
import co.topl.genusLibrary.failure.Failures.NoBlockHeaderFoundOnNodeFailure
import co.topl.genusLibrary.failure.Failures.NonExistentTransactionsFailure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.model.HeightData
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
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
          .returning(Option.empty[BlockId].pure[F])
          .once()

        assertIO(
          nodeBlockFetcher fetch height,
          HeightData(height = height, blockData = None).asRight
        )

      }
    }
  }

  test(
    "On a block without a header, a Left of NoBlockHeaderFoundOnNodeFailure should be returned"
  ) {
    PropF.forAllF { (height: Long, blockId: BlockId) =>
      withMock {

        (toplRpc.blockIdAtHeight _)
          .expects(height)
          .returning(blockId.some.pure[F])
          .once()

        (toplRpc.fetchBlockHeader _)
          .expects(blockId)
          .returning(Option.empty[BlockHeader].pure[F])
          .once()

        assertIO(
          nodeBlockFetcher fetch height,
          NoBlockHeaderFoundOnNodeFailure(blockId).asLeft
        )

      }
    }
  }

  test(
    "On a block without a body, a Left of NoBlockBodyFoundOnNodeFailure should be returned"
  ) {
    PropF.forAllF { (height: Long, blockId: BlockId, blockHeader: BlockHeader) =>
      withMock {

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
          .returning(Option.empty[BlockBody].pure[F])
          .once()

        assertIO(
          nodeBlockFetcher fetch height,
          NoBlockBodyFoundOnNodeFailure(blockId).asLeft
        )

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
        blockId:       BlockId,
        blockHeader:   BlockHeader,
        transactionId: IoTransaction32
      ) =>
        withMock {

          val blockBody = BlockBody.of(Seq(transactionId))

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
            .expects(transactionId)
            .returning(Option.empty[IoTransaction].pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            NonExistentTransactionsFailure(ListSet(transactionId)).asLeft
          )

        }
    }
  }

  test(
    "On a block with three transactions and missing two of them, " +
    "a Left of NonExistentTransactionsFailure with the missing txIds should be returned"
  ) {
    PropF.forAllF {
      (
        height:           Long,
        blockId:          BlockId,
        blockHeader:      BlockHeader,
        transactionId_01: IoTransaction32,
        transactionId_02: IoTransaction32,
        transactionId_03: IoTransaction32,
        transaction_01:   IoTransaction
      ) =>
        withMock {

          val blockBody = BlockBody.of(
            Seq(
              transactionId_01,
              transactionId_02,
              transactionId_03
            )
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
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_02)
            .returning(Option.empty[IoTransaction].pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_03)
            .returning(Option.empty[IoTransaction].pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            NonExistentTransactionsFailure(
              ListSet(
                transactionId_02,
                transactionId_03
              )
            ).asLeft
          )

        }
    }
  }

  test(
    "On a block with three transactions and missing all of them, " +
    "a Left of NonExistentTransactionsFailure with the missing txIds should be returned"
  ) {
    PropF.forAllF {
      (
        height:           Long,
        blockId:          BlockId,
        blockHeader:      BlockHeader,
        transactionId_01: IoTransaction32,
        transactionId_02: IoTransaction32,
        transactionId_03: IoTransaction32
      ) =>
        withMock {

          val blockBody = BlockBody.of(
            Seq(
              transactionId_01,
              transactionId_02,
              transactionId_03
            )
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
            .returning(Option.empty[IoTransaction].pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_02)
            .returning(Option.empty[IoTransaction].pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_03)
            .returning(Option.empty[IoTransaction].pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            NonExistentTransactionsFailure(
              ListSet(
                transactionId_01,
                transactionId_02,
                transactionId_03
              )
            ).asLeft
          )

        }
    }
  }

  test(
    "On a block with a header and three transactions, a Right of the full block body should be returned"
  ) {
    PropF.forAllF {
      (
        height:         Long,
        blockId:        BlockId,
        blockHeader:    BlockHeader,
        transaction_01: IoTransaction,
        transaction_02: IoTransaction,
        transaction_03: IoTransaction
      ) =>
        withMock {

          val transactionId_01 = transaction_01.id
          val transactionId_02 = transaction_02.id
          val transactionId_03 = transaction_03.id

          val blockBody = BlockBody(
            Seq(
              transactionId_01,
              transactionId_02,
              transactionId_03
            )
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
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_02)
            .returning(transaction_02.some.pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(transactionId_03)
            .returning(transaction_03.some.pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            HeightData(
              height = height,
              blockData = BlockData(
                header = blockHeader,
                body = blockBody,
                transactions = Chain(transaction_01, transaction_02, transaction_03)
              ).some
            ).asRight
          )

        }
    }
  }

}
