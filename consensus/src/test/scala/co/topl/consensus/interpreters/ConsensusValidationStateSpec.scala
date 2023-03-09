package co.topl.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.testInterpreters.TestStore
import co.topl.consensus.interpreters.EpochBoundariesEventSourcedState.EpochBoundaries
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SignatureKesProduct
import co.topl.eventtree.EventSourcedState
import co.topl.models._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.ModelGenerators.stakingAddressGen
import co.topl.models.utility.Ratio
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class ConsensusValidationStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val arbitraryStakingAddress: Arbitrary[StakingAddress] = Arbitrary(stakingAddressGen)

  test("Retrieve relative stakes at epoch N-2 of the requested block for epoch N > 1") {
    PropF.forAllF {
      (
        genesisId: BlockId,
        n2Id:      BlockId,
        nId:       BlockId,
        address:   StakingAddress
      ) =>
        withMock {
          val slot = 5
          for {
            boundaryStore <- TestStore.make[F, Epoch, BlockId]
            consensusData <- (
              TestStore.make[F, StakingAddress, BigInt],
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, StakingAddress, SignatureKesProduct]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- boundaryStore.put(3L, n2Id)
            _ <- consensusData.operatorStakes.put(address, 1)
            _ <- consensusData.totalActiveStake.put((), 5)
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

            _ <- underTest.operatorRelativeStake(nId, slot)(address).assertEquals(Some(Ratio(1, 5)))

          } yield ()

        }
    }
  }

  test("Retrieve registrations at epoch N-2 of the requested block for epoch N > 1") {
    PropF.forAllF {
      (
        genesisId:    BlockId,
        n2Id:         BlockId,
        nId:          BlockId,
        address:      StakingAddress,
        registration: SignatureKesProduct
      ) =>
        withMock {
          val slot = 5
          for {
            boundaryStore <- TestStore.make[F, Epoch, BlockId]
            consensusData <- (
              TestStore.make[F, StakingAddress, BigInt],
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, StakingAddress, SignatureKesProduct]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- boundaryStore.put(3L, n2Id)
            _ <- consensusData.operatorStakes.put(address, 1)
            _ <- consensusData.registrations.put(address, registration)
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

            _ <- underTest.operatorRegistration(nId, slot)(address).assertEquals(Some(registration))

          } yield ()

        }
    }
  }

  test("Retrieve relative stakes at the big bang block for epoch N <= 1") {
    PropF.forAllF {
      (
        bigBangId: BlockId,
        nId:       BlockId,
        address:   StakingAddress
      ) =>
        withMock {
          val slot = 3
          for {
            boundaryStore <- TestStore.make[F, Epoch, BlockId]
            consensusData <- (
              TestStore.make[F, StakingAddress, BigInt],
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, StakingAddress, SignatureKesProduct]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- consensusData.operatorStakes.put(address, 1)
            _ <- consensusData.totalActiveStake.put((), 5)
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

            _ <- underTest.operatorRelativeStake(nId, slot)(address).assertEquals(Some(Ratio(1, 5)))

          } yield ()

        }
    }
  }

  test("Retrieve registrations at the genesis block for epoch N <= 1") {
    PropF.forAllF {
      (
        bigBangId:    BlockId,
        nId:          BlockId,
        address:      StakingAddress,
        registration: SignatureKesProduct
      ) =>
        withMock {
          val slot = 3
          for {
            boundaryStore <- TestStore.make[F, Epoch, BlockId]
            consensusData <- (
              TestStore.make[F, StakingAddress, BigInt],
              TestStore.make[F, Unit, BigInt],
              TestStore.make[F, StakingAddress, SignatureKesProduct]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- consensusData.registrations.put(address, registration)
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

            _ <- underTest.operatorRegistration(nId, slot)(address).assertEquals(Some(registration))

          } yield ()

        }
    }
  }
}
