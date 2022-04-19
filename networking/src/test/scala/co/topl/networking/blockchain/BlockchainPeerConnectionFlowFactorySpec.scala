package co.topl.networking.blockchain

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import cats.effect.IO
import cats.implicits._
import co.topl.catsakka._
import co.topl.models.TypedIdentifier
import co.topl.networking.NetworkGen._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.SortedSet

class BlockchainPeerConnectionFlowFactorySpec
    extends TestKit(ActorSystem("BlockchainPeerConnectionFlowFactorySpec"))
    with AnyFlatSpecLike
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
      val server = mock[BlockchainPeerServer[F]]

      (() => server.localBlockAdoptions)
        .expects()
        .once()
        .returning(Source.never[TypedIdentifier].pure[F])

      val factory = BlockchainPeerConnectionFlowFactory.createFactory[F](server)

      val (protocols, _) = factory.protocolsForPeer(connectedPeer, connectionLeader).unsafeRunSync()

      protocols.length shouldBe 10L
      protocols.map(_.sessionId).toNes[Byte].toSortedSet shouldBe SortedSet[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    }
  }
}
