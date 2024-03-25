package co.topl.genus.interpreter

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{LockAddress, TransactionId, TransactionInputAddress, TransactionOutputAddress}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genus.algebras.VertexFetcherAlgebra
import co.topl.genus.interpreter.GraphTransactionFetcher
import co.topl.genus.model.{GE, GEs}
import co.topl.genus.orientDb.OrientThread
import co.topl.genus.orientDb.instances.{SchemaBlockHeader, SchemaIoTransaction, SchemaTxo}
import co.topl.genus.services._
import co.topl.models.generators.consensus.ModelGenerators._
import com.tinkerpop.blueprints.{Direction, Vertex}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GraphTransactionFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("On fetchTransaction with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchTransaction _)
            .expects(transactionId)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransaction(transactionId),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
              .asLeft[Option[IoTransaction]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransaction if an empty iterator is returned, None IoTransaction should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (vertexFetcher.fetchTransaction _)
            .expects(transactionId)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransaction(transactionId),
            Option.empty[IoTransaction].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransaction if IoTransactionVertex exist, Some IoTransaction should be returned") {

    PropF.forAllF { (transactionId: TransactionId, ioTransaction: IoTransaction) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex                                   <- mock[Vertex].pure[F].toResource

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaIoTransaction.Field.Transaction)
            .once()
            .returning(ioTransaction.toByteArray)

          _ = (vertexFetcher.fetchTransaction _)
            .expects(transactionId)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransaction(transactionId),
            Some(ioTransaction).asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionReceipt with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchTransaction _)
            .expects(transactionId)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionReceipt(transactionId),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransaction", expectedTh): GE)
              .asLeft[Option[TransactionReceipt]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionReceipt if an empty iterator is returned, None IoTransaction should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (vertexFetcher.fetchTransaction _)
            .expects(transactionId)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionReceipt(transactionId),
            Option.empty[TransactionReceipt].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionReceipt if IoTransactionVertex exist, Some TransactionReceipt should be returned") {

    PropF.forAllF { (transactionId: TransactionId, ioTransaction: IoTransaction, blockHeader: BlockHeader) =>
      withMock {

        val blockHeaderVertex = mock[Vertex]
        val iotxVertex = mock[Vertex]

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource

          _ = (vertexFetcher.fetchTransaction _)
            .expects(transactionId)
            .once()
            .returning(Option(iotxVertex).asRight[GE].pure[F])

          _ = (iotxVertex.getProperty[Vertex] _)
            .expects(SchemaIoTransaction.Field.ParentBlock)
            .once()
            .returning(blockHeaderVertex)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.BlockId)
            .once()
            .returning(blockHeader.embedId.id.value.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long] _)
            .expects(SchemaBlockHeader.Field.ParentSlot)
            .once()
            .returning(blockHeader.parentSlot)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.EligibilityCertificate)
            .once()
            .returning(blockHeader.eligibilityCertificate.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.OperationalCertificate)
            .once()
            .returning(blockHeader.operationalCertificate.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.Address)
            .once()
            .returning(blockHeader.address.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.Metadata)
            .once()
            .returning(blockHeader.metadata.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.TxRoot)
            .once()
            .returning(blockHeader.txRoot.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.BloomFilter)
            .once()
            .returning(blockHeader.bloomFilter.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.ParentHeaderId)
            .once()
            .returning(blockHeader.parentHeaderId.value.toByteArray)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long] _)
            .expects(SchemaBlockHeader.Field.Slot)
            .once()
            .returning(blockHeader.slot)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long] _)
            .expects(SchemaBlockHeader.Field.Height)
            .once()
            .returning(blockHeader.height)

          _ = (blockHeaderVertex
            .getProperty[java.lang.Long] _)
            .expects(SchemaBlockHeader.Field.Timestamp)
            .once()
            .returning(blockHeader.timestamp)

          _ = (blockHeaderVertex
            .getProperty[Array[Byte]] _)
            .expects(SchemaBlockHeader.Field.Version)
            .once()
            .returning(blockHeader.version.toByteArray)

          _ = (iotxVertex.getProperty[Array[Byte]] _)
            .expects(SchemaIoTransaction.Field.Transaction)
            .once()
            .returning(ioTransaction.toByteArray)

          expectedTransactionReceipt =
            TransactionReceipt(
              ioTransaction,
              ConfidenceFactor.defaultInstance,
              blockHeader.id,
              ChainDistance(blockHeader.height)
            )

          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionReceipt(transactionId),
            Some(expectedTransactionReceipt).asRight[GE]
          ).toResource

        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionsByAddress with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (vertexFetcher.fetchLockAddress _)
            .expects(lockAddress)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchTransactionsByAddress", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactionsByAddress", expectedTh): GE)
              .asLeft[List[Txo]]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionsByAddress with empty response, a empty seq should be returned") {

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {

        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex = mock[Vertex]

          _ = (vertexFetcher.fetchLockAddress _)
            .expects(lockAddress)
            .once()
            .returning(Option(vertex).asRight[GE].pure[F])

          _ = (vertex
            .getVertices(_: Direction, _: String))
            .expects(*, *)
            .once()
            .returning(new java.util.LinkedList[Vertex]())

          graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
          _ <- assertIO(
            graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT),
            List.empty[Txo].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTransactionsByAddress with UNSPENT filter, a empty seq should be returned") {

    PropF.forAllF {
      (
        lockAddress:              LockAddress,
        transactionOutputAddress: TransactionOutputAddress,
        transactionOutput:        UnspentTransactionOutput
      ) =>
        withMock {

          val res = for {
            implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
            vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            lockAddressVertex = mock[Vertex]
            txoVertex = mock[Vertex]

            _ = (vertexFetcher.fetchLockAddress _)
              .expects(lockAddress)
              .once()
              .returning(Option(lockAddressVertex).asRight[GE].pure[F])

            _ = (txoVertex.getProperty[Array[Byte]] _)
              .expects(SchemaTxo.Field.TransactionOutput)
              .once()
              .returning(transactionOutput.toByteArray)

            _ = (txoVertex.getProperty[java.lang.Integer] _)
              .expects(SchemaTxo.Field.State)
              .once()
              .returning(TxoState.UNSPENT.value)

            _ = (txoVertex.getProperty[java.lang.Integer] _)
              .expects(SchemaTxo.Field.SpendingInputIndex)
              .once()
              .returning(null)

            _ = (txoVertex.getProperty[Array[Byte]] _)
              .expects(SchemaTxo.Field.OutputAddress)
              .once()
              .returning(transactionOutputAddress.toByteArray)

            getVerticesRes = new java.util.LinkedList[Vertex]
            _ = getVerticesRes.add(txoVertex)
            _ = (lockAddressVertex
              .getVertices(_: Direction, _: String))
              .expects(*, *)
              .once()
              .returning(getVerticesRes)

            graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
            _ <- assertIO(
              graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT),
              List.empty[Txo].asRight[GE]
            ).toResource
          } yield ()

          res.use_
        }

    }
  }

  test("On fetchTransactionsByAddress with SPENT filter, a seq with 1 item should be returned") {

    PropF.forAllF {
      (
        lockAddress:              LockAddress,
        transactionOutputAddress: TransactionOutputAddress,
        transactionOutput:        UnspentTransactionOutput,
        spendingTransactionInput: SpentTransactionOutput
      ) =>
        withMock {

          val res = for {
            implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
            vertexFetcher                            <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            lockAddressVertex = mock[Vertex]
            spendingTransactionVertex = mock[Vertex]
            txoVertex = mock[Vertex]
            spendingTransaction = IoTransaction.defaultInstance.withInputs(List(spendingTransactionInput))

            _ = (vertexFetcher.fetchLockAddress _)
              .expects(lockAddress)
              .once()
              .returning(Option(lockAddressVertex).asRight[GE].pure[F])

            _ = (txoVertex.getProperty[Array[Byte]] _)
              .expects(SchemaTxo.Field.TransactionOutput)
              .once()
              .returning(transactionOutput.toByteArray)

            _ = (txoVertex.getProperty[java.lang.Integer] _)
              .expects(SchemaTxo.Field.State)
              .once()
              .returning(TxoState.SPENT.value)

            _ = (txoVertex.getProperty[java.lang.Integer] _)
              .expects(SchemaTxo.Field.SpendingInputIndex)
              .once()
              .returning(0)

            _ = (txoVertex.getProperty[Vertex] _)
              .expects(SchemaTxo.Field.SpendingTransaction)
              .once()
              .returning(spendingTransactionVertex)

            _ = (spendingTransactionVertex.getProperty[Array[Byte]] _)
              .expects(SchemaIoTransaction.Field.Transaction)
              .once()
              .returning(spendingTransaction.toByteArray)

            _ = (txoVertex.getProperty[Array[Byte]] _)
              .expects(SchemaTxo.Field.OutputAddress)
              .once()
              .returning(transactionOutputAddress.toByteArray)

            getVerticesRes = new java.util.LinkedList[Vertex]
            _ = getVerticesRes.add(txoVertex)
            _ = (lockAddressVertex
              .getVertices(_: Direction, _: String))
              .expects(*, *)
              .once()
              .returning(getVerticesRes)

            graphTransactionFetcher <- GraphTransactionFetcher.make[F](vertexFetcher)
            txos <- EitherT(graphTransactionFetcher.fetchTransactionByLockAddress(lockAddress, TxoState.SPENT))
              .valueOrF(IO.raiseError)
              .toResource
            _ <- IO.pure(txos.length).assertEquals(1).toResource
            txo = txos.head
            _ <- IO
              .pure(txo.spender.get.inputAddress)
              .assertEquals(
                TransactionInputAddress(
                  network = spendingTransactionInput.address.network,
                  ledger = spendingTransactionInput.address.ledger,
                  index = 0,
                  id = spendingTransaction.id
                )
              )
              .toResource
            _ <- IO.pure(txo.spender.get.input).assertEquals(spendingTransactionInput).toResource
          } yield ()

          res.use_
        }

    }
  }

}
