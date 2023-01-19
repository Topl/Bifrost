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
import co.topl.models.ModelGenerators._
import co.topl.models.generators.brambl.ModelGenerators._
import co.topl.models.generators.models.ModelGenerators.arbitraryTransaction
import co.topl.models._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.genusLibrary.model.{BlockData, HeightData}
import co.topl.typeclasses.implicits._
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
    PropF.forAllF { (height: Long, blockId: TypedIdentifier) =>
      withMock {

        (toplRpc.blockIdAtHeight _)
          .expects(height)
          .returning(blockId.some.pure[F])
          .once()

        (toplRpc.fetchBlockHeader _)
          .expects(blockId)
          .returning(Option.empty[co.topl.consensus.models.BlockHeader].pure[F])
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
    PropF.forAllF { (height: Long, blockId: TypedIdentifier, blockHeader: co.topl.consensus.models.BlockHeader) =>
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
          .returning(Option.empty[co.topl.node.models.BlockBody].pure[F])
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
        blockId:       TypedIdentifier,
        blockHeader:   co.topl.consensus.models.BlockHeader,
        transactionId: co.topl.brambl.models.Identifier.IoTransaction32
      ) =>
        withMock {

          val blockBody: co.topl.node.models.BlockBody =
            co.topl.node.models.BlockBody.of(Seq(transactionId))

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
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId))
            .returning(Option.empty[co.topl.proto.models.Transaction].pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            NonExistentTransactionsFailure(ListSet(co.topl.models.TypedBytes.ioTx32(transactionId))).asLeft
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
        blockId:          TypedIdentifier,
        blockHeader:      co.topl.consensus.models.BlockHeader,
        transactionId_01: co.topl.brambl.models.Identifier.IoTransaction32,
        transactionId_02: co.topl.brambl.models.Identifier.IoTransaction32,
        transactionId_03: co.topl.brambl.models.Identifier.IoTransaction32,
        transaction_01:   co.topl.proto.models.Transaction
      ) =>
        withMock {

          val blockBody = co.topl.node.models.BlockBody.of(
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
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId_01))
            .returning(transaction_01.some.pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId_02))
            .returning(Option.empty[co.topl.proto.models.Transaction].pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId_03))
            .returning(Option.empty[co.topl.proto.models.Transaction].pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            NonExistentTransactionsFailure(
              ListSet(
                co.topl.models.TypedBytes.ioTx32(transactionId_02),
                co.topl.models.TypedBytes.ioTx32(transactionId_03)
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
        blockId:          TypedIdentifier,
        blockHeader:      co.topl.consensus.models.BlockHeader,
        transactionId_01: co.topl.brambl.models.Identifier.IoTransaction32,
        transactionId_02: co.topl.brambl.models.Identifier.IoTransaction32,
        transactionId_03: co.topl.brambl.models.Identifier.IoTransaction32
      ) =>
        withMock {

          val blockBody: co.topl.node.models.BlockBody = co.topl.node.models.BlockBody.of(
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
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId_01))
            .returning(Option.empty[co.topl.proto.models.Transaction].pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId_02))
            .returning(Option.empty[co.topl.proto.models.Transaction].pure[F])
            .once()

          (toplRpc.fetchTransaction _)
            .expects(co.topl.models.TypedBytes.ioTx32(transactionId_03))
            .returning(Option.empty[co.topl.proto.models.Transaction].pure[F])
            .once()

          assertIO(
            nodeBlockFetcher fetch height,
            NonExistentTransactionsFailure(
              ListSet(
                co.topl.models.TypedBytes.ioTx32(transactionId_01),
                co.topl.models.TypedBytes.ioTx32(transactionId_02),
                co.topl.models.TypedBytes.ioTx32(transactionId_03)
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
        blockId:        TypedIdentifier,
        blockHeader:    co.topl.consensus.models.BlockHeader,
        transaction_01: co.topl.proto.models.Transaction,
        transaction_02: co.topl.proto.models.Transaction,
        transaction_03: co.topl.proto.models.Transaction
      ) =>
        withMock {

          val transactionId_01: TypedIdentifier = transaction_01.id.asTypedBytes
          val transactionId_02: TypedIdentifier = transaction_02.id.asTypedBytes
          val transactionId_03: TypedIdentifier = transaction_03.id.asTypedBytes

          // TODO move and rename in case we need to translate this model in other place
          def aux(tx: TypedIdentifier): co.topl.brambl.models.Identifier.IoTransaction32 =
            co.topl.brambl.models.Identifier.IoTransaction32.of(
              Some(
                co.topl.brambl.models.Evidence.Sized32.of(
                  Some(
                    quivr.models.Digest.Digest32.of(com.google.protobuf.ByteString.copyFrom(tx.dataBytes.toArray))
                  )
                )
              )
            )

          val blockBody: co.topl.node.models.BlockBody = co.topl.node.models.BlockBody.of(
            Seq(
              aux(transactionId_01),
              aux(transactionId_02),
              aux(transactionId_03)
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
