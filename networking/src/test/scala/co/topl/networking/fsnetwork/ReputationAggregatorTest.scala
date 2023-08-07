package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.models.ModelGenerators.GenHelper
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregatorTest.{defaultP2PConfig, F}
import co.topl.networking.fsnetwork.TestHelper.arbitraryHost
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, SECONDS}

object ReputationAggregatorTest {
  type F[A] = IO[A]
  val defaultSlotDuration: FiniteDuration = FiniteDuration(1, SECONDS)

  val defaultP2PConfig: P2PNetworkConfig =
    P2PNetworkConfig(NetworkProperties(), defaultSlotDuration)
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
      val initialNewMap = Map(hostToRemove -> 1L)
      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PeerIsCold(hostToRemove))
            _ = assert(!newState.performanceReputation.contains(hostToRemove))
            _ = assert(!newState.blockProvidingReputation.contains(hostToRemove))
            _ = assert(!newState.noveltyReputation.contains(hostToRemove))
            newState2 <- actor.send(ReputationAggregator.Message.PeerIsCold(hostToRemove))
            _ = assert(!newState2.performanceReputation.contains(hostToRemove))
            _ = assert(!newState2.blockProvidingReputation.contains(hostToRemove))
            _ = assert(!newState2.noveltyReputation.contains(hostToRemove))
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
      val initialNewMap = Map(host -> 1L)

      val delay = 230L
      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PingPongMessagePing(host, Right(delay)))
            _ = assert(
              newState.performanceReputation(host) == ReputationAggregator.delayToReputation(defaultP2PConfig, delay)
            )
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
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
      val initialNewMap = Map(host -> 1L)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.BanPeer(host))
        .returns(().pure[F])
      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PingPongMessagePing(host, Left(NoPongMessage)))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
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
      val initialNewMap = Map(host -> 1L)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.BanPeer(host))
        .returns(().pure[F])
      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.PingPongMessagePing(host, Left(IncorrectPongMessage)))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
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
      val initialNewMap = Map(host -> 1L)

      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.BanPeer(host))
        .returns(().pure[F])
      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.HostProvideIncorrectBlock(host))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
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
      val initialNewMap = Map(host -> 1L)

      val downloadTime = 120
      val reputation = ReputationAggregator.delayToReputation(defaultP2PConfig, downloadTime)

      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.DownloadTimeHeader(host, downloadTime))
            _ = assert(newState.performanceReputation(host) == (initialReputation * 2 + reputation) / 3.0)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("Performance reputation after body downloading shall be updated") {
    PropF.forAllF { (txTimes: Seq[Long]) =>
      withMock {
        val peersManager = mock[PeersManagerActor[F]]
        val host = arbitraryHost.arbitrary.first
        val txDownloadTime: Seq[Long] = txTimes.filter(_ > 0)

        val initialReputation = 0.5
        val initialPerfMap = Map(host -> initialReputation, arbitraryHost.arbitrary.first -> 0.1)
        val initialBlockMap = Map(arbitraryHost.arbitrary.first -> 0.1, host -> 0.7)
        val initialNewMap = Map(host -> 1L)

        val downloadTime: Long = 120
        val reputation = ReputationAggregator.delayToReputation(defaultP2PConfig, (txDownloadTime :+ downloadTime).max)

        ReputationAggregator
          .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
          .use { actor =>
            for {
              newState <- actor.send(ReputationAggregator.Message.DownloadTimeBody(host, downloadTime, txDownloadTime))
              _ = assert(newState.performanceReputation(host) == (initialReputation * 2 + reputation) / 3.0)
              _ = assert(newState.blockProvidingReputation == initialBlockMap)
              _ = assert(newState.noveltyReputation == initialNewMap)
            } yield ()
          }
      }
    }
  }

  test("Reputation shall be updated after added new host") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialReputation = 0.5
      val initialPerfMap =
        Map(arbitraryHost.arbitrary.first -> initialReputation, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap =
        Map(arbitraryHost.arbitrary.first -> 0.1, arbitraryHost.arbitrary.first -> 0.7)
      val initialNewMap =
        Map(arbitraryHost.arbitrary.first -> 1L)

      val reputation = defaultP2PConfig.remotePeerNoveltyInSlots

      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.NewHotPeer(NonEmptyChain.one(host)))
            _ = assert(newState.performanceReputation == (initialPerfMap + (host -> 0.0)))
            _ = assert(newState.blockProvidingReputation == (initialBlockMap + (host -> 0.0)))
            _ = assert(newState.noveltyReputation == (initialNewMap + (host -> reputation)))
          } yield ()
        }
    }
  }

  test("Block providing reputation: take better value from update and current value") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host1 = arbitraryHost.arbitrary.first
      val host2 = arbitraryHost.arbitrary.first
      val host3 = arbitraryHost.arbitrary.first
      val host4 = arbitraryHost.arbitrary.first

      val worstKnownSources = 3L
      val worstReputation = ReputationAggregator.knownSourcesToReputation(defaultP2PConfig, worstKnownSources)

      val okKnownSources = 2L
      val okReputation = ReputationAggregator.knownSourcesToReputation(defaultP2PConfig, okKnownSources)

      val bestKnownSources = 1L
      val bestReputation = ReputationAggregator.knownSourcesToReputation(defaultP2PConfig, bestKnownSources)
      assert(bestReputation == 1)

      val initialBlockMap =
        Map(host1 -> worstReputation, host2 -> okReputation, host3 -> bestReputation)
      val update = List(
        host1 -> okKnownSources,
        host1 -> bestKnownSources,
        host2 -> worstKnownSources,
        host2 -> bestKnownSources,
        host3 -> worstKnownSources,
        host3 -> okKnownSources,
        host4 -> worstKnownSources
      )

      val blockProvidingUpdate =
        ReputationAggregator.Message.BlockProvidingReputationUpdate(NonEmptyChain.fromSeq(update).get)

      val initialPerfMap =
        Map(arbitraryHost.arbitrary.first -> 0.4, arbitraryHost.arbitrary.first -> 0.1)
      val initialNewMap =
        Map(arbitraryHost.arbitrary.first -> 1L)

      val expectedBlockProvidingReputation =
        Map(host1 -> bestReputation, host2 -> bestReputation, host3 -> bestReputation, host4 -> worstReputation)
      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(blockProvidingUpdate)
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
            _ = assert(newState.blockProvidingReputation == expectedBlockProvidingReputation)
          } yield ()
        }
    }
  }

  test("Reputation shall be set to zero if remote host provide bad k lookback slot data") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      val host = arbitraryHost.arbitrary.first

      val initialReputation = 0.5
      val initialPerfMap =
        Map(arbitraryHost.arbitrary.first -> initialReputation, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap =
        Map(arbitraryHost.arbitrary.first -> 0.1, arbitraryHost.arbitrary.first -> 0.7)
      val initialNewMap =
        Map(arbitraryHost.arbitrary.first -> 1L)

      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap + (host -> 1.0), initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.BadKLookbackSlotData(host))
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap + (host -> 0.0))
            _ = assert(newState.noveltyReputation == initialNewMap)
          } yield ()
        }
    }
  }

  test("Host reputation shall be sent for warm hosts update by request") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]

      val initialReputation = 0.5
      val initialPerfMap =
        Map(arbitraryHost.arbitrary.first -> initialReputation, arbitraryHost.arbitrary.first -> 0.1)
      val initialBlockMap =
        Map(arbitraryHost.arbitrary.first -> 0.1, arbitraryHost.arbitrary.first -> 0.7)
      val initialNewMap =
        Map(arbitraryHost.arbitrary.first -> 1L)

      (peersManager.sendNoWait _).expects(PeersManager.Message.UpdateWarmHosts(initialPerfMap)).returns(().pure[F])

      ReputationAggregator
        .makeActor(peersManager, defaultP2PConfig, initialPerfMap, initialBlockMap, initialNewMap)
        .use { actor =>
          for {
            newState <- actor.send(ReputationAggregator.Message.UpdateWarmHosts)
            _ = assert(newState.performanceReputation == initialPerfMap)
            _ = assert(newState.blockProvidingReputation == initialBlockMap)
            _ = assert(newState.noveltyReputation == initialNewMap)
          } yield ()
        }
    }
  }
}
