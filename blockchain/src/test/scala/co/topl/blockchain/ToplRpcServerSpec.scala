package co.topl.blockchain

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.{MempoolAlgebra, TransactionSyntaxValidationAlgebra}
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockBodyV2, BlockHeaderV2, SlotData, Transaction, TypedIdentifier}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream

class ToplRpcServerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  type F[A] = IO[A]

  test("Fetch Block ID At Height") {
    PropF.forAllF { (_canonicalHead: SlotData, targetBlockId: TypedIdentifier) =>
      val canonicalHead = _canonicalHead.copy(height = 10)
      // Test where requested height is less than canonical head height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          blockHeights = mock[EventSourcedState[F, Long => F[Option[TypedIdentifier]]]]
          _ = (blockHeights
            .useStateAt[Option[TypedIdentifier]](_: TypedIdentifier)(
              _: (Long => F[Option[TypedIdentifier]]) => F[Option[TypedIdentifier]]
            ))
            .expects(canonicalHead.slotId.blockId, *)
            .once()
            .onCall { case (_, _) =>
              targetBlockId.some.pure[F]
            }
          underTest <- createServer(localChain = localChain, blockHeights = blockHeights)
          _         <- underTest.blockIdAtHeight(canonicalHead.height - 1).assertEquals(targetBlockId.some)
        } yield ()
      } >>
      // Test where requested height equals canonical head height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          blockHeights = mock[EventSourcedState[F, Long => F[Option[TypedIdentifier]]]]
          underTest <- ToplRpcServer.make[F](
            mock[Store[F, TypedIdentifier, BlockHeaderV2]],
            mock[Store[F, TypedIdentifier, BlockBodyV2]],
            mock[Store[F, TypedIdentifier, Transaction]],
            mock[MempoolAlgebra[F]],
            mock[TransactionSyntaxValidationAlgebra[F]],
            localChain,
            blockHeights,
            mock[ParentChildTree[F, TypedIdentifier]],
            Stream.empty
          )
          _ <- underTest.blockIdAtHeight(canonicalHead.height).assertEquals(canonicalHead.slotId.blockId.some)
        } yield ()
      } >>
      // Test where requested height is greater than canonical head height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          blockHeights = mock[EventSourcedState[F, Long => F[Option[TypedIdentifier]]]]
          underTest <- ToplRpcServer.make[F](
            mock[Store[F, TypedIdentifier, BlockHeaderV2]],
            mock[Store[F, TypedIdentifier, BlockBodyV2]],
            mock[Store[F, TypedIdentifier, Transaction]],
            mock[MempoolAlgebra[F]],
            mock[TransactionSyntaxValidationAlgebra[F]],
            localChain,
            blockHeights,
            mock[ParentChildTree[F, TypedIdentifier]],
            Stream.empty
          )
          _ <- underTest.blockIdAtHeight(canonicalHead.height + 1).assertEquals(None)
        } yield ()
      } >>
      // Test where requested height is invalid
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          blockHeights = mock[EventSourcedState[F, Long => F[Option[TypedIdentifier]]]]
          underTest <- ToplRpcServer.make[F](
            mock[Store[F, TypedIdentifier, BlockHeaderV2]],
            mock[Store[F, TypedIdentifier, BlockBodyV2]],
            mock[Store[F, TypedIdentifier, Transaction]],
            mock[MempoolAlgebra[F]],
            mock[TransactionSyntaxValidationAlgebra[F]],
            localChain,
            blockHeights,
            mock[ParentChildTree[F, TypedIdentifier]],
            Stream.empty
          )
          _ <- interceptIO[IllegalArgumentException](underTest.blockIdAtHeight(0))
          _ <- interceptIO[IllegalArgumentException](underTest.blockIdAtHeight(-1))
        } yield ()
      }
    }
  }

  test("Fetch Block ID At Depth") {
    PropF.forAllF { (_canonicalHead: SlotData, targetBlockId: TypedIdentifier) =>
      val canonicalHead = _canonicalHead.copy(height = 10)
      // Test where requested depth is greater than 0 but less than chain height
      withMock {
        for {
          localChain <- mock[LocalChainAlgebra[F]].pure[F]
          _ = (() => localChain.head).expects().once().returning(canonicalHead.pure[F])
          blockHeights = mock[EventSourcedState[F, Long => F[Option[TypedIdentifier]]]]
          _ = (blockHeights
            .useStateAt[Option[TypedIdentifier]](_: TypedIdentifier)(
              _: (Long => F[Option[TypedIdentifier]]) => F[Option[TypedIdentifier]]
            ))
            .expects(canonicalHead.slotId.blockId, *)
            .once()
            .onCall { case (_, _) =>
              targetBlockId.some.pure[F]
            }
          underTest <- createServer(localChain = localChain, blockHeights = blockHeights)
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
          parentChildTree <- mock[ParentChildTree[F, TypedIdentifier]].pure[F]
          _ = (
            (
              a,
              b
            ) => parentChildTree.findCommonAncestor(a, b)
          ).expects(slotHead.slotId.blockId, slotA.slotId.blockId)
            .once()
            .returning(
              (NonEmptyChain.one(slotA.slotId.blockId), NonEmptyChain(slotA.slotId.blockId, slotB.slotId.blockId))
                .pure[F]
            )
          underTest <- createServer(
            localChain = localChain,
            blockIdTree = parentChildTree,
            localBlockAdoptionsStream = Stream.eval(slotA.slotId.blockId.pure[F])
          )
          stream <- underTest.synchronizationTraversal()
          // find common ancestor is inclusive, and synchronizationTraversal tail results
          expected = SynchronizationTraversalSteps.Applied(slotB.slotId.blockId)
          _ <- stream.compile.toList.map(_.contains(expected)).assert
        } yield ()
      }
    }
  }

  private def createServer(
    headerStore:         Store[F, TypedIdentifier, BlockHeaderV2] = mock[Store[F, TypedIdentifier, BlockHeaderV2]],
    bodyStore:           Store[F, TypedIdentifier, BlockBodyV2] = mock[Store[F, TypedIdentifier, BlockBodyV2]],
    transactionStore:    Store[F, TypedIdentifier, Transaction] = mock[Store[F, TypedIdentifier, Transaction]],
    mempool:             MempoolAlgebra[F] = mock[MempoolAlgebra[F]],
    syntacticValidation: TransactionSyntaxValidationAlgebra[F] = mock[TransactionSyntaxValidationAlgebra[F]],
    localChain:          LocalChainAlgebra[F] = mock[LocalChainAlgebra[F]],
    blockHeights: EventSourcedState[F, Long => F[Option[TypedIdentifier]]] =
      mock[EventSourcedState[F, Long => F[Option[TypedIdentifier]]]],
    blockIdTree:               ParentChildTree[F, TypedIdentifier] = mock[ParentChildTree[F, TypedIdentifier]],
    localBlockAdoptionsStream: Stream[F, TypedIdentifier] = Stream.empty
  ) =
    ToplRpcServer
      .make[F](
        headerStore,
        bodyStore,
        transactionStore,
        mempool,
        syntacticValidation,
        localChain,
        blockHeights,
        blockIdTree,
        localBlockAdoptionsStream
      )
}
