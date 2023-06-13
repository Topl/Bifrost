package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.algebras.{BlockHeaderToBodyValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.eventtree.ParentChildTree
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.BlockChecker.BlockCheckerActor
import co.topl.networking.fsnetwork.PeerActorTest.F
import co.topl.networking.fsnetwork.ReputationAggregator.Message.PingPongMessagePing
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.fsnetwork.RequestsProxy.RequestsProxyActor
import co.topl.node.models.{PingMessage, PongMessage}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerActorTest {
  type F[A] = IO[A]
}

class PeerActorTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"

  test("Ping shall be started for warm hosts and sent result to reputation aggregator") {
    withMock {
      val blockChecker = mock[BlockCheckerActor[F]]
      val requestsProxy = mock[RequestsProxyActor[F]]
      val localChain = mock[LocalChainAlgebra[F]]
      val slotDataStore = mock[Store[F, BlockId, SlotData]]
      val transactionStore = mock[Store[F, TransactionId, IoTransaction]]
      val blockIdTree = mock[ParentChildTree[F, BlockId]]
      val headerToBodyValidation = mock[BlockHeaderToBodyValidationAlgebra[F]]
      val pingPongInterval = FiniteDuration(100, MILLISECONDS)
      val pingDelay = 10

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Thread.sleep(pingDelay)
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).atLeastOnce().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay)
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }

      PeerActor
        .makeActor(
          hostId,
          client,
          blockChecker,
          requestsProxy,
          reputationAggregation,
          localChain,
          slotDataStore,
          transactionStore,
          blockIdTree,
          headerToBodyValidation,
          pingPongInterval
        )
        .use { actor =>
          for {
            _ <- actor.send(PeerActor.Message.UpdateState(PeerState.Warm))
            _ = Thread.sleep(pingPongInterval.toMillis + pingDelay * 5)
          } yield ()
        }
    }

  }
}
