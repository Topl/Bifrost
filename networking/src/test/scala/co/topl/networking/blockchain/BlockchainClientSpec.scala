package co.topl.networking.blockchain

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.crypto.hash.Blake2b256
import co.topl.node.models.BlockBody
import co.topl.networking.p2p.ConnectedPeer
import com.google.protobuf.ByteString
import fs2._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.charset.StandardCharsets

class BlockchainClientSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  type F[A] = IO[A]

  test("trace a common ancestor") {
    PropF.forAllF(Gen.posNum[Long]) { headHeight =>
      PropF.forAllF(Gen.chooseNum[Long](1, headHeight)) { ancestorHeight =>
        val client = new BlockchainPeerClient[F] {
          def remotePeer: F[ConnectedPeer] = ???

          def remotePeerAdoptions: F[Stream[F, BlockId]] = ???

          def remoteTransactionNotifications: F[Stream[F, TransactionId]] = ???

          def getRemoteSlotData(id: BlockId): F[Option[SlotData]] = ???

          def getRemoteHeader(id: BlockId): F[Option[BlockHeader]] = ???

          def getRemoteBody(id: BlockId): F[Option[BlockBody]] = ???

          def getRemoteTransaction(id: TransactionId): F[Option[IoTransaction]] = ???

          def getRemoteBlockIdAtHeight(
            height:       Long,
            localBlockId: Option[BlockId]
          ): F[Option[BlockId]] =
            (height.toString + (if (height > ancestorHeight) "remote" else "")).typedId.some
              .pure[F]

          override def getRemoteBlockIdAtDepth(depth: Long): F[Option[BlockId]] = ???
        }

        val blockHeights =
          (height: Long) =>
            (height.toString + (if (height > ancestorHeight) "local" else "")).typedId
              .pure[F]

        client.findCommonAncestor(blockHeights, () => headHeight.pure[F]).assertEquals(ancestorHeight.toString.typedId)
      }
    }
  }

  implicit private class StringToBlockId(string: String) {
    def typedId: BlockId = BlockId(ByteString.copyFrom(new Blake2b256().hash(string.getBytes(StandardCharsets.UTF_8))))
  }

}
