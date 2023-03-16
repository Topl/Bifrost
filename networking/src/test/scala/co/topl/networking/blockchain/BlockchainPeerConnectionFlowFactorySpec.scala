package co.topl.networking.blockchain

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.consensus.models.BlockId
import co.topl.networking.NetworkGen._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import fs2._
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.SortedSet

class BlockchainPeerConnectionFlowFactorySpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  behavior of "BlockchainPeerConnectionFlowFactory"

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  type F[A] = IO[A]

  it should "produce 10 typed protocols" in {

    forAll { (connectedPeer: ConnectedPeer, connectionLeader: ConnectionLeader) =>
      val server = mock[BlockchainPeerServerAlgebra[F]]

      (() => server.localBlockAdoptions)
        .expects()
        .once()
        .returning(Stream.never[F].pure[F]: F[Stream[F, BlockId]])

      (() => server.localTransactionNotifications)
        .expects()
        .once()
        .returning(Stream.never[F].pure[F]: F[Stream[F, Identifier.IoTransaction32]])

      val factory = BlockchainPeerConnectionFlowFactory.createFactory[F](server)

      val (protocols, _) = factory.protocolsForPeer(connectedPeer, connectionLeader).unsafeRunSync()

      protocols.length shouldBe 14L
      protocols.map(_.sessionId).toNes[Byte].toSortedSet shouldBe SortedSet[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
        13, 14)
    }
  }
}
