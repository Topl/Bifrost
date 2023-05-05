package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.models.{LockAddress, TransactionId, TransactionOutputAddress}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.genus.services.{TransactionReceipt, Txo, TxoState}
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.instances.{SchemaIoTransaction, SchemaTxo}
import com.tinkerpop.blueprints.{Direction, Vertex}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphTransactionFetcherTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("On fetchTransaction with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (transactionId: TransactionId) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
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
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
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
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          vertex        <- mock[Vertex].pure[F].toResource

          _ = (vertex.getPropertyKeys _)
            .expects()
            .once()
            .returning(
              java.util.Set.of(
                SchemaIoTransaction.Field.TransactionId,
                SchemaIoTransaction.Field.Transaction
              )
            )

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaIoTransaction.Field.Transaction)
            .once()
            .returning(ioTransaction.toByteArray)

          _ = (vertex.getProperty[Array[Byte]] _)
            .expects(SchemaIoTransaction.Field.TransactionId)
            .once()
            .returning(ioTransaction.id.value.toByteArray)

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
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
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
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
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

  test("On fetchTransactionsByAddress with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {

        val res = for {
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
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
            graphTransactionFetcher.fetchTransactionsByAddress(lockAddress, TxoState.SPENT),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactionsByAddress", expectedTh): GE)
              .asLeft[Seq[Txo]]
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
          vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
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
            graphTransactionFetcher.fetchTransactionsByAddress(lockAddress, TxoState.SPENT),
            Seq.empty[Txo].asRight[GE]
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
            vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            lockAddressVertex = mock[Vertex]
            txoVertex = mock[Vertex]

            _ = (vertexFetcher.fetchLockAddress _)
              .expects(lockAddress)
              .once()
              .returning(Option(lockAddressVertex).asRight[GE].pure[F])

            _ = (() => txoVertex.getPropertyKeys)
              .expects()
              .once()
              .returning(
                java.util.Set.of(
                  SchemaTxo.Field.TransactionOutput,
                  SchemaTxo.Field.State,
                  SchemaTxo.Field.OutputAddress
                )
              )

            _ = (txoVertex.getProperty[Array[Byte]] _)
              .expects(SchemaTxo.Field.TransactionOutput)
              .once()
              .returning(transactionOutput.toByteArray)

            _ = (txoVertex.getProperty[java.lang.Integer] _)
              .expects(SchemaTxo.Field.State)
              .once()
              .returning(TxoState.UNSPENT.value)

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
              graphTransactionFetcher.fetchTransactionsByAddress(lockAddress, TxoState.SPENT),
              Seq.empty[Txo].asRight[GE]
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
        transactionOutput:        UnspentTransactionOutput
      ) =>
        withMock {

          val res = for {
            vertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
            lockAddressVertex = mock[Vertex]
            txoVertex = mock[Vertex]

            _ = (vertexFetcher.fetchLockAddress _)
              .expects(lockAddress)
              .once()
              .returning(Option(lockAddressVertex).asRight[GE].pure[F])

            _ = (() => txoVertex.getPropertyKeys)
              .expects()
              .once()
              .returning(
                java.util.Set.of(
                  SchemaTxo.Field.TransactionOutput,
                  SchemaTxo.Field.State,
                  SchemaTxo.Field.OutputAddress
                )
              )

            _ = (txoVertex.getProperty[Array[Byte]] _)
              .expects(SchemaTxo.Field.TransactionOutput)
              .once()
              .returning(transactionOutput.toByteArray)

            _ = (txoVertex.getProperty[java.lang.Integer] _)
              .expects(SchemaTxo.Field.State)
              .once()
              .returning(TxoState.SPENT.value)

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
              graphTransactionFetcher.fetchTransactionsByAddress(lockAddress, TxoState.SPENT).map(_.map(_.size)),
              1.asRight[GE]
            ).toResource
          } yield ()

          res.use_
        }

    }
  }

}
