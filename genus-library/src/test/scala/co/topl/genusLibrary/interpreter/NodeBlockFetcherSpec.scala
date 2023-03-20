package co.topl.genusLibrary.interpreter

import cats.data.Chain
import cats.effect.IO
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.model.GenusExceptions._
import co.topl.genusLibrary.model.{BlockData, HeightData}
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import fs2._
import scala.collection.immutable.ListSet

class NodeBlockFetcherSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private val toplRpc: ToplRpc[F, Stream[F, *]] = mock[ToplRpc[F, Stream[F, *]]]

  private val nodeBlockFetcher = NodeBlockFetcher.make[F](toplRpc)

  test("On no block at given height, a None should be returned") {
    PropF.forAllF { height: Long =>
      withMock {

        (toplRpc.blockIdAtHeight _)
          .expects(height)
          .returning(Option.empty[BlockId].pure[F])
          .once()

        val res = for {
          fetcher <- nodeBlockFetcher
          _ <- assertIO(
            fetcher fetch height,
            HeightData(height = height, blockData = None).asRight
          ).toResource

        } yield ()
        res.use_

      }
    }
  }

  test("On a block without a header, a Left of NoBlockHeaderFoundOnNode should be returned") {
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

        val res = for {
          fetcher <- nodeBlockFetcher
          _ <- assertIO(
            fetcher fetch height,
            NoBlockHeaderFoundOnNode(blockId).asLeft
          ).toResource

        } yield ()
        res.use_

      }
    }
  }

  test("On a block without a body, a Left of NoBlockBodyFoundOnNode should be returned") {
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

        val res = for {
          fetcher <- nodeBlockFetcher
          _ <- assertIO(
            fetcher fetch height,
            NoBlockBodyFoundOnNode(blockId).asLeft
          ).toResource

        } yield ()
        res.use_

      }
    }
  }

  test(
    "On a block with a transaction and missing it, " +
    "a Left of NonExistentTransactions with that txId should be returned"
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

          val res = for {
            fetcher <- nodeBlockFetcher
            _ <- assertIO(
              fetcher fetch height,
              NonExistentTransactions(ListSet(transactionId)).asLeft
            ).toResource

          } yield ()
          res.use_

        }
    }
  }

  test(
    "On a block with three transactions and missing two of them, " +
    "a Left of NonExistentTransactions with the missing txIds should be returned"
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

          val res = for {
            fetcher <- nodeBlockFetcher
            _ <- assertIO(
              fetcher fetch height,
              NonExistentTransactions(
                ListSet(
                  transactionId_02,
                  transactionId_03
                )
              ).asLeft
            ).toResource

          } yield ()
          res.use_

        }
    }
  }

  test(
    "On a block with three transactions and missing all of them, " +
    "a Left of NonExistentTransactions with the missing txIds should be returned"
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

          val res = for {
            fetcher <- nodeBlockFetcher
            _ <- assertIO(
              fetcher fetch height,
              NonExistentTransactions(
                ListSet(
                  transactionId_01,
                  transactionId_02,
                  transactionId_03
                )
              ).asLeft
            ).toResource

          } yield ()
          res.use_

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

          val res = for {
            fetcher <- nodeBlockFetcher
            _ <- assertIO(
              fetcher fetch height,
              HeightData(
                height = height,
                blockData = BlockData(
                  header = blockHeader,
                  body = blockBody,
                  transactions = Chain(transaction_01, transaction_02, transaction_03)
                ).some
              ).asRight
            ).toResource

          } yield ()
          res.use_

        }
    }
  }

}
