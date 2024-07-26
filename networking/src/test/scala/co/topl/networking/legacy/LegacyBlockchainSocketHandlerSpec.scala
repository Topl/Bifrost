package co.topl.networking.legacy

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.networking.NetworkGen._
import co.topl.networking.blockchain.BlockchainPeerServerAlgebra
import co.topl.networking.p2p.ConnectedPeer
import fs2._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.SortedSet

class LegacyBlockchainSocketHandlerSpec extends CatsEffectSuite with AsyncMockFactory with ScalaCheckEffectSuite {

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  type F[A] = IO[A]

  test("produce typed protocols") {
    PropF.forAllF { (connectedPeer: ConnectedPeer, socketLeader: ConnectionLeader) =>
      withMock {
        val server = mock[BlockchainPeerServerAlgebra[F]]

        (() => server.localBlockAdoptions)
          .expects()
          .once()
          .returning(Stream.never[F].pure[F]: F[Stream[F, BlockId]])

        (() => server.localTransactionNotifications)
          .expects()
          .once()
          .returning(Stream.never[F].pure[F]: F[Stream[F, TransactionId]])

        val factory = LegacyBlockchainSocketHandler.createFactory[F](server, Applicative[F].unit)
        for {
          (protocols, _) <- factory.protocolsForPeer(connectedPeer, socketLeader)
          _ = assert(protocols.length == 24)
          protocolSessionIds = protocols.map(_.sessionId).toNes[Byte].toSortedSet
          expectedProtocolSessionIds = SortedSet[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
            19, 20, 21, 22, 23, 24)
          _ = assert(protocolSessionIds == expectedProtocolSessionIds)
        } yield ()
      }
    }
  }

  implicit val arbitraryConnectionLeader: Arbitrary[ConnectionLeader] =
    Arbitrary(
      Gen.oneOf(ConnectionLeader.Local, ConnectionLeader.Remote)
    )
}
