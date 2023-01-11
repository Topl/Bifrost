package co.topl.networking.blockchain

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.models._
import co.topl.networking.p2p.ConnectedPeer
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

          def remotePeerAdoptions: F[Stream[F, TypedIdentifier]] = ???

          def remoteTransactionNotifications: F[Stream[F, TypedIdentifier]] = ???

          def getRemoteSlotData(id: TypedIdentifier): F[Option[SlotData]] = ???

          def getRemoteHeader(id: TypedIdentifier): F[Option[BlockHeader]] = ???

          def getRemoteBody(id: TypedIdentifier): F[Option[BlockBody]] = ???

          def getRemoteTransaction(id: TypedIdentifier): F[Option[Transaction]] = ???

          def getRemoteBlockIdAtHeight(
            height:       Long,
            localBlockId: Option[TypedIdentifier]
          ): F[Option[TypedIdentifier]] =
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
    def typedId: TypedIdentifier = TypedBytes(1: Byte, Bytes.encodeUtf8(string).value)
  }

}
