package co.topl.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.testInterpreters.TestStore
import co.topl.consensus.interpreters.EpochBoundariesEventSourcedState.EpochBoundaries
import co.topl.consensus.models._
import co.topl.eventtree.EventSourcedState
import co.topl.models._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.numerics.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class ConsensusValidationStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Retrieve registrations at epoch N-2 of the requested block for epoch N > 1") {
    PropF.forAllF {
      (
        genesisId:    BlockId,
        n2Id:         BlockId,
        nId:          BlockId,
        registration: StakingRegistration
      ) =>
        withMock {
          val staker = ActiveStaker(registration, 5)
          val slot = 5
          for {
            boundaryStore <- TestStore.make[F, Epoch, BlockId]
            consensusData <- (
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, StakingAddress, ActiveStaker]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- boundaryStore.put(3L, n2Id)
            _ <- consensusData.stakers.put(staker.registration.address, staker)
            epochBoundaryEventSourcedState = new EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
              F
            ], BlockId] {
              def stateAt(eventId: BlockId): F[EpochBoundaries[F]] = ???

              def useStateAt[U](eventId: BlockId)(f: EpochBoundaries[F] => F[U]): F[U] = {
                assert(eventId == nId)
                f(boundaryStore)
              }
            }
            consensusDataEventSourcedState = new EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
              F
            ], BlockId] {
              def stateAt(eventId: BlockId): F[ConsensusDataEventSourcedState.ConsensusData[F]] = ???

              def useStateAt[U](
                eventId: BlockId
              )(f: ConsensusDataEventSourcedState.ConsensusData[F] => F[U]): F[U] = {
                assert(eventId == n2Id)
                f(consensusData)
              }
            }
            clock = mock[ClockAlgebra[F]]

            _ = (() => clock.slotsPerEpoch)
              .expects()
              .once()
              .returning(1L.pure[F])

            underTest <- ConsensusValidationState
              .make[F](genesisId, epochBoundaryEventSourcedState, consensusDataEventSourcedState, clock)

            _ <- underTest.staker(nId, slot)(staker.registration.address).assertEquals(Some(staker))

          } yield ()

        }
    }
  }

  test("Retrieve registrations at the genesis block for epoch N <= 1") {
    PropF.forAllF {
      (
        bigBangId:    BlockId,
        nId:          BlockId,
        registration: StakingRegistration
      ) =>
        withMock {
          val staker = ActiveStaker(registration, 5)
          val slot = 3
          for {
            boundaryStore <- TestStore.make[F, Epoch, BlockId]
            consensusData <- (
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, StakingAddress, ActiveStaker]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- consensusData.stakers.put(staker.registration.address, staker)
            epochBoundaryEventSourcedState = new EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
              F
            ], BlockId] {
              def stateAt(eventId: BlockId): F[EpochBoundaries[F]] = ???

              def useStateAt[U](eventId: BlockId)(f: EpochBoundaries[F] => F[U]): F[U] = {
                assert(eventId == nId)
                f(boundaryStore)
              }
            }
            consensusDataEventSourcedState = new EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
              F
            ], BlockId] {
              def stateAt(eventId: BlockId): F[ConsensusDataEventSourcedState.ConsensusData[F]] = ???

              def useStateAt[U](
                eventId: BlockId
              )(f: ConsensusDataEventSourcedState.ConsensusData[F] => F[U]): F[U] = {
                assert(eventId == bigBangId)
                f(consensusData)
              }
            }
            clock = mock[ClockAlgebra[F]]

            _ = (() => clock.slotsPerEpoch)
              .expects()
              .once()
              .returning(2L.pure[F])

            underTest <- ConsensusValidationState
              .make[F](bigBangId, epochBoundaryEventSourcedState, consensusDataEventSourcedState, clock)

            _ <- underTest.staker(nId, slot)(staker.registration.address).assertEquals(Some(staker))

          } yield ()

        }
    }
  }
}
