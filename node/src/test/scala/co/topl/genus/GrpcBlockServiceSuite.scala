package co.topl.genus

import cats.effect.IO
import cats.implicits._
import co.topl.typeclasses.implicits._
import co.topl.consensus.models._
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators._
import co.topl.node.models._
import io.grpc.{Metadata, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GrpcBlockServiceSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("getBlockById: OK") {
    PropF.forAllF { (blockId: BlockId, blockHeader: BlockHeader, blockBody: BlockBody) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        val blockData = BlockData(blockHeader, blockBody, Seq.empty)

        (blockFetcher.fetchBlock _)
          .expects(blockId)
          .once()
          .returning(blockData.some.asRight[GE].pure[F])

        for {
          res <- underTest.getBlockById(GetBlockByIdRequest(blockId), new Metadata())
          _ = assert(
            res == BlockResponse(
              FullBlock(blockData.header, fullBody = FullBlockBody(blockData.transactions))
            )
          )
        } yield ()
      }
    }
  }

  test("getBlockById: Not Found") {
    PropF.forAllF { (blockId: BlockId) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        (blockFetcher.fetchBlock _)
          .expects(blockId)
          .once()
          .returning(Option.empty[BlockData].asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: BlockId:${blockId.show}")(
            underTest.getBlockById(GetBlockByIdRequest(blockId), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockById: Exceptions") {
    PropF.forAllF { (blockId: BlockId) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        (blockFetcher.fetchBlock _)
          .expects(blockId)
          .once()
          .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[Option[BlockData]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getBlockById(GetBlockByIdRequest(blockId), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByHeight: OK") {
    PropF.forAllF { (height: Long, blockHeader: BlockHeader, blockBody: BlockBody) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        val blockData = BlockData(blockHeader, blockBody, Seq.empty)

        (blockFetcher.fetchBlockByHeight _)
          .expects(height)
          .once()
          .returning(blockData.some.asRight[GE].pure[F])

        for {
          res <- underTest.getBlockByHeight(GetBlockByHeightRequest(ChainDistance(height)), new Metadata())
          _ = assert(
            res == BlockResponse(
              FullBlock(blockData.header, fullBody = FullBlockBody(blockData.transactions))
            )
          )
        } yield ()
      }
    }
  }

  test("getBlockByHeight: Not Found") {
    PropF.forAllF { (height: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        (blockFetcher.fetchBlockByHeight _)
          .expects(height)
          .once()
          .returning(Option.empty[BlockData].asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: Height:${height.show}")(
            underTest.getBlockByHeight(GetBlockByHeightRequest(ChainDistance(height)), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByHeight: Exceptions") {
    PropF.forAllF { (height: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        (blockFetcher.fetchBlockByHeight _)
          .expects(height)
          .once()
          .returning((GEs.UnImplemented: GE).asLeft[Option[BlockData]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: An implementation is missing")(
            underTest.getBlockByHeight(GetBlockByHeightRequest(ChainDistance(height)), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByDepth: OK") {
    PropF.forAllF { (depth: Long, blockHeader: BlockHeader, blockBody: BlockBody) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        val blockData = BlockData(blockHeader, blockBody, Seq.empty)

        (blockFetcher.fetchBlockByDepth _)
          .expects(depth)
          .once()
          .returning(blockData.some.asRight[GE].pure[F])

        for {
          res <- underTest.getBlockByDepth(GetBlockByDepthRequest(ChainDistance(depth)), new Metadata())
          _ = assert(
            res == BlockResponse(
              FullBlock(blockData.header, fullBody = FullBlockBody(blockData.transactions))
            )
          )
        } yield ()
      }
    }
  }

  test("getBlockByDepth: Not Found") {
    PropF.forAllF { (depth: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        (blockFetcher.fetchBlockByDepth _)
          .expects(depth)
          .once()
          .returning(Option.empty[BlockData].asRight[GE].pure[F])

        for {
          _ <- interceptMessageIO[StatusException](s"NOT_FOUND: Depth:${depth.show}")(
            underTest.getBlockByDepth(GetBlockByDepthRequest(ChainDistance(depth)), new Metadata())
          )
        } yield ()
      }
    }

  }

  test("getBlockByDepth: Exceptions") {
    PropF.forAllF { (depth: Long) =>
      withMock {
        val blockFetcher = mock[BlockFetcherAlgebra[F]]
        val underTest = new GrpcBlockService[F](blockFetcher)

        (blockFetcher.fetchBlockByDepth _)
          .expects(depth)
          .once()
          .returning((GEs.InternalMessage("Boom!"): GE).asLeft[Option[BlockData]].pure[F])

        for {
          _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
            underTest.getBlockByDepth(GetBlockByDepthRequest(ChainDistance(depth)), new Metadata())
          )
        } yield ()
      }
    }

  }

}
