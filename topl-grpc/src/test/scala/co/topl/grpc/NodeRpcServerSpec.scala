package co.topl.grpc

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, ProtocolConfigurationAlgebra, Store}
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.blockchain.{BlockchainCore, DataStores, Validators}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.consensus.Consensus
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.Ledger
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import co.topl.proto.node.{EpochData, NodeConfig}
import fs2.Stream
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class NodeRpcServerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Fetch Block ID At Height") {
    PropF.forAllF { (_canonicalHead: SlotData, targetBlockId: BlockId) =>
      val canonicalHead = _canonicalHead.copy(height = 10)
      // Test where requested height is less than canonical head height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          _ = (localChain
            .blockIdAtHeight(_: Long))
            .expects(canonicalHead.height - 1)
            .once()
            .returning(targetBlockId.some.pure[F])
          underTest <- createServer(localChain = localChain)
          _         <- underTest.blockIdAtHeight(canonicalHead.height - 1).assertEquals(targetBlockId.some)
        } yield ()
      } >>
      // Test where requested height equals canonical head height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          underTest <- createServer(localChain = localChain)
          _ <- underTest
            .blockIdAtHeight(canonicalHead.height)
            .assertEquals(canonicalHead.slotId.blockId.some)
        } yield ()
      } >>
      // Test where requested height is greater than canonical head height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          underTest <- createServer(localChain = localChain)
          _         <- underTest.blockIdAtHeight(canonicalHead.height + 1).assertEquals(None)
        } yield ()
      } >>
      // Test where requested height is invalid
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          underTest  <- createServer(localChain = localChain)
          _          <- interceptIO[IllegalArgumentException](underTest.blockIdAtHeight(0))
          _          <- interceptIO[IllegalArgumentException](underTest.blockIdAtHeight(-1))
        } yield ()
      }
    }
  }

  test("Fetch Block ID At Depth") {
    PropF.forAllF { (_canonicalHead: SlotData, targetBlockId: BlockId) =>
      val canonicalHead = _canonicalHead.copy(height = 10)
      // Test where requested depth is greater than 0 but less than chain height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          _ = (localChain.blockIdAtHeight(_: Long)).expects(*).once().returning(targetBlockId.some.pure[F])
          underTest <- createServer(localChain = localChain)
          _         <- underTest.blockIdAtDepth(1).assertEquals(targetBlockId.some)
        } yield ()
      } >>
      // Test where requested depth is 0
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          underTest <- createServer(localChain = localChain)
          _         <- underTest.blockIdAtDepth(0).assertEquals(canonicalHead.slotId.blockId.some)
        } yield ()
      } >>
      // Test where requested depth is greater than chain height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          underTest <- createServer(localChain = localChain)
          _         <- underTest.blockIdAtDepth(canonicalHead.height + 1).assertEquals(None)
        } yield ()
      } >>
      // Test where requested depth is invalid
      withMock {
        for {
          underTest <- createServer()
          _         <- interceptIO[IllegalArgumentException](underTest.blockIdAtDepth(-1))
        } yield ()
      }
    }
  }

  test("Fetch Adoptions Stream Rpc Synchronization Traversal") {
    import cats.data.NonEmptyChain
    import co.topl.algebras.SynchronizationTraversalSteps

    PropF.forAllF { (slotHead: SlotData, slotA: SlotData, slotB: SlotData) =>
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(slotHead.pure[F])
          _ = (() => localChain.adoptions)
            .expects()
            .once()
            .returning(Stream.eval((slotA.slotId.blockId).pure[F]).pure[F])
          parentChildTree <- mock[ParentChildTree[F, BlockId]].pure[F]
          _ = (
            (
              a,
              b
            ) => parentChildTree.findCommonAncestor(a, b)
          ).expects(slotHead.slotId.blockId, slotA.slotId.blockId)
            .once()
            .returning(
              (
                NonEmptyChain.one(slotA.slotId.blockId),
                NonEmptyChain(slotA.slotId.blockId, slotB.slotId.blockId)
              )
                .pure[F]
            )
          underTest <- createServer(
            localChain = localChain,
            blockIdTree = parentChildTree
          )
          stream <- underTest.synchronizationTraversal()
          // find common ancestor is inclusive, and synchronizationTraversal tail results
          expected = SynchronizationTraversalSteps.Applied(slotB.slotId.blockId)
          _ <- stream.compile.toList.map(_.contains(expected)).assert
        } yield ()
      }
    }
  }

  test("Fetch fetchProtocolConfigs Stream Rpc") {

    withMock {
      for {
        protocolConfigs <- mock[ProtocolConfigurationAlgebra[F, Stream[F, *]]].pure[F]
        nodeConfig = Seq(NodeConfig(0, 100, 300), NodeConfig(1, 200, 600))
        _ = (() => protocolConfigs.fetchNodeConfig)
          .expects()
          .once()
          .returning(Stream.emits(nodeConfig).pure[F])

        underTest <- createServer(
          protocolConfiguration = protocolConfigs
        )
        stream <- underTest.fetchProtocolConfigs()
        _      <- stream.compile.toList.map(_.size == 2).assert
        _      <- stream.compile.toList.map(_.contains(nodeConfig.head)).assert
        _      <- stream.compile.toList.map(_.contains(nodeConfig.tail.head)).assert
      } yield ()
    }

    test("Fetch EpochData Rpc") {

      withMock {
        for {
          epochDataAlgebra <- mock[EpochDataAlgebra[F]].pure[F]
          epochData = EpochData.defaultInstance
          _ = (epochDataAlgebra.dataOf _)
            .expects(0L)
            .once()
            .returning(epochData.some.pure[F])

          underTest <- createServer(
            epochData = epochDataAlgebra
          )
          _ <- underTest.fetchEpochData(0L.some).map(_.isDefined).assert
          _ <- underTest.fetchEpochData(Option.empty[Long]).map(_.isDefined).assert
        } yield ()
      }
    }

  }

  private def createServer(
    headerStore:         Store[F, BlockId, BlockHeader] = mock[Store[F, BlockId, BlockHeader]],
    bodyStore:           Store[F, BlockId, BlockBody] = mock[Store[F, BlockId, BlockBody]],
    transactionStore:    Store[F, TransactionId, IoTransaction] = mock[Store[F, TransactionId, IoTransaction]],
    mempool:             MempoolAlgebra[F] = mock[MempoolAlgebra[F]],
    syntacticValidation: TransactionSyntaxVerifier[F] = mock[TransactionSyntaxVerifier[F]],
    localChain:          LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]],
    blockIdTree:         ParentChildTree[F, BlockId] = mock[ParentChildTree[F, BlockId]],
    protocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]] =
      mock[ProtocolConfigurationAlgebra[F, Stream[F, *]]],
    epochData: EpochDataAlgebra[F] = mock[EpochDataAlgebra[F]],
    clock:     ClockAlgebra[F] = mock[ClockAlgebra[F]]
  ) = {
    val dataStores = mock[DataStores[F]]
    (() => dataStores.headers).expects().anyNumberOfTimes().returning(headerStore)
    (() => dataStores.bodies).expects().anyNumberOfTimes().returning(bodyStore)
    (() => dataStores.transactions).expects().anyNumberOfTimes().returning(transactionStore)

    val validators = mock[Validators[F]]
    (() => validators.transactionSyntax).expects().anyNumberOfTimes().returning(syntacticValidation)

    val consensus = mock[Consensus[F]]
    (() => consensus.localChain).expects().anyNumberOfTimes().returning(localChain)

    val ledger = mock[Ledger[F]]
    (() => ledger.mempool).expects().anyNumberOfTimes().returning(mempool)

    val blockchain = mock[BlockchainCore[F]]
    (() => blockchain.blockIdTree).expects().anyNumberOfTimes().returning(blockIdTree)
    (() => blockchain.protocolConfiguration).expects().anyNumberOfTimes().returning(protocolConfiguration)
    (() => blockchain.epochData).expects().anyNumberOfTimes().returning(epochData)
    (() => blockchain.clock).expects().anyNumberOfTimes().returning(clock)
    (() => blockchain.dataStores).expects().anyNumberOfTimes().returning(dataStores)
    (() => blockchain.validators).expects().anyNumberOfTimes().returning(validators)
    (() => blockchain.consensus).expects().anyNumberOfTimes().returning(consensus)
    (() => blockchain.ledger).expects().anyNumberOfTimes().returning(ledger)

    ToplRpcServer.make[F](blockchain)
  }
}
