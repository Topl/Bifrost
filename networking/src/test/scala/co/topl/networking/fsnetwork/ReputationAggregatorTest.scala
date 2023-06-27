package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.models.ModelGenerators.GenHelper
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregatorTest.F
import co.topl.networking.fsnetwork.TestHelper.arbitraryHost
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object ReputationAggregatorTest {
  type F[A] = IO[A]
}

class ReputationAggregatorTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  test("Reputation shall be removed by request") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val hostToRemove = arbitraryHost.arbitrary.first

      val initialPerfMap = Map(hostToRemove -> 0.5, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, hostToRemove -> 0.7)
      val initialNewMap = Map(hostToRemove -> 0.5)
      ReputationAggregator
        .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.RemoveReputationForHost(hostToRemove))
            _ = assert(!newState.performanceReputation.contains(hostToRemove))
            _ = assert(!newState.blockProvidingReputation.contains(hostToRemove))
            _ = assert(!newState.newnessReputation.contains(hostToRemove))
            newState2 <- actor.send(ReputationAggregator.Message.RemoveReputationForHost(hostToRemove))
            _ = assert(!newState2.performanceReputation.contains(hostToRemove))
            _ = assert(!newState2.blockProvidingReputation.contains(hostToRemove))
            _ = assert(!newState2.newnessReputation.contains(hostToRemove))
          } yield ()
        }
    }
  }

  test("Correct pong message shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialPerfMap = Map(host -> 0.5, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
      val initialNewMap = Map(host -> 0.5)

      val delay = 230
      ReputationAggregator
        .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PingPongMessagePing(host, Right(delay)))
            _ = assert(newState.performanceReputation(host) == ReputationAggregator.delayToReputation(delay))
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.newnessReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("NoPongMessage message shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialPerfMap = Map(host -> 0.5, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
      val initialNewMap = Map(host -> 0.5)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatePeerStatus(host, PeerState.Banned))
        .returns(().pure[F])
      ReputationAggregator
        .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PingPongMessagePing(host, Left(NoPongMessage)))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.newnessReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("IncorrectPongMessage message shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialPerfMap = Map(host -> 0.5, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
      val initialNewMap = Map(host -> 0.5)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatePeerStatus(host, PeerState.Banned))
        .returns(().pure[F])
      ReputationAggregator
        .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PingPongMessagePing(host, Left(IncorrectPongMessage)))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.newnessReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("IncorrectBlock message shall be processed") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialPerfMap = Map(host -> 0.5, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
      val initialNewMap = Map(host -> 0.5)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatePeerStatus(host, PeerState.Banned))
        .returns(().pure[F])
      ReputationAggregator
        .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.HostProvideIncorrectBlock(host))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.newnessReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("Performance reputation after header downloading shall be updated") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialReputation = 0.5
      val initialPerfMap = Map(host -> initialReputation, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
      val initialNewMap = Map(host -> 0.5)

      val downloadTime = 120
      val reputation = ReputationAggregator.delayToReputation(downloadTime)

      ReputationAggregator
        .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.DownloadTimeHeader(host, downloadTime))
            _ = assert(newState.performanceReputation(host) == (initialReputation * 2 + reputation) / 3.0)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.newnessReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("Performance reputation after body downloading shall be updated") {
    PropF.forAllF { (txDownloadTime: Seq[Long]) =>
      withMock {
        val peersManager = mock[PeersManagerActor[F]]
        val host = arbitraryHost.arbitrary.first

        val initialReputation = 0.5
        val initialPerfMap = Map(host -> initialReputation, arbitraryHost.arbitrary.first -> 0.1)
        val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
        val initialNewMap = Map(host -> 0.5)

        val downloadTime: Long = 120
        val reputation = ReputationAggregator.delayToReputation((txDownloadTime :+ downloadTime).max)

        ReputationAggregator
          .makeActor(peersManager, initialPerfMap, initialBlockMap, initialNewMap)
          .use { actor =>
            for {
              newState <- actor.send(ReputationAggregator.Message.DownloadTimeBody(host, downloadTime, txDownloadTime))
              _ = assert(newState.performanceReputation(host) == (initialReputation * 2 + reputation) / 3.0)
              _ = assert(newState.blockProvidingReputation == initialBlockMap)
              _ = assert(newState.newnessReputation == initialNewMap)
            } yield ()
          }
      }
    }
  }
}
