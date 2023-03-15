package co.topl.networking.blockchain

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.node.models.BlockBody
import co.topl.networking.p2p.ConnectedPeer
import com.google.protobuf.ByteString
import fs2._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class BlockchainClientSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with EitherValues
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  behavior of "BlockchainClient"

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  type F[A] = IO[A]

  it should "trace a common ancestor" in {
    forAll(Gen.posNum[Long]) { headHeight =>
      forAll(Gen.chooseNum[Long](1, headHeight)) { ancestorHeight =>
        val client = new BlockchainPeerClient[F] {
          def remotePeer: F[ConnectedPeer] = ???

          def remotePeerAdoptions: F[Stream[F, BlockId]] = ???

          def remoteTransactionNotifications: F[Stream[F, Identifier.IoTransaction32]] = ???

          def getRemoteSlotData(id: BlockId): F[Option[SlotData]] = ???

          def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] = ???

          def getRemoteBody(id: BlockId): F[Option[BlockBody]] = ???

          def getRemoteTransaction(id: Identifier.IoTransaction32): F[Option[IoTransaction]] = ???

          def getRemoteBlockIdAtHeight(
            height:       Long,
            localBlockId: Option[BlockId]
          ): F[Option[BlockId]] =
            (height.toString + (if (height > ancestorHeight) "remote" else "")).typedId.some
              .pure[F]
        }

        val blockHeights =
          (height: Long) =>
            (height.toString + (if (height > ancestorHeight) "local" else "")).typedId
              .pure[F]

        val ancestor = client.findCommonAncestor(blockHeights, () => headHeight.pure[F]).unsafeRunSync()

        ancestor shouldBe ancestorHeight.toString.typedId
      }
    }
  }

  implicit private class StringToBlockId(string: String) {
    def typedId: BlockId = BlockId(ByteString.copyFromUtf8(string))
  }

}
