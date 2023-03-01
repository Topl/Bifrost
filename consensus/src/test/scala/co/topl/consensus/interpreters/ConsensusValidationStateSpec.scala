package co.topl.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.testInterpreters.TestStore
import co.topl.consensus.interpreters.EpochBoundariesEventSourcedState.EpochBoundaries
import co.topl.eventtree.EventSourcedState
import co.topl.models.utility.Ratio
import co.topl.models.{Box, Epoch, Int128, StakingAddresses, TypedIdentifier}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import co.topl.numerics.implicits._
import co.topl.models.ModelGenerators._

class ConsensusValidationStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Retrieve relative stakes at epoch N-2 of the requested block for epoch N > 1") {
    PropF.forAllF {
      (
        genesisId: TypedIdentifier,
        n2Id:      TypedIdentifier,
        nId:       TypedIdentifier,
        address:   StakingAddresses.Operator
      ) =>
        withMock {
          val slot = 5
          for {
            boundaryStore <- TestStore.make[F, Epoch, TypedIdentifier]
            consensusData <- (
              TestStore.make[F, StakingAddresses.Operator, Int128],
              TestStore.make[F, Unit, Int128],
              TestStore.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- boundaryStore.put(3L, n2Id)
            _ <- consensusData.operatorStakes.put(address, 1)
            _ <- consensusData.totalActiveStake.put((), 5)
            epochBoundaryEventSourcedState = new EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[EpochBoundaries[F]] = ???

              def useStateAt[U](eventId: TypedIdentifier)(f: EpochBoundaries[F] => F[U]): F[U] = {
                assert(eventId == nId)
                f(boundaryStore)
              }
            }
            consensusDataEventSourcedState = new EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[ConsensusDataEventSourcedState.ConsensusData[F]] = ???

              def useStateAt[U](
                eventId: TypedIdentifier
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
        genesisId:    TypedIdentifier,
        n2Id:         TypedIdentifier,
        nId:          TypedIdentifier,
        address:      StakingAddresses.Operator,
        registration: Box.Values.Registrations.Operator
      ) =>
        withMock {
          val slot = 5
          for {
            boundaryStore <- TestStore.make[F, Epoch, TypedIdentifier]
            consensusData <- (
              TestStore.make[F, StakingAddresses.Operator, Int128],
              TestStore.make[F, Unit, Int128],
              TestStore.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- boundaryStore.put(3L, n2Id)
            _ <- consensusData.operatorStakes.put(address, 1)
            _ <- consensusData.registrations.put(address, registration)
            epochBoundaryEventSourcedState = new EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[EpochBoundaries[F]] = ???

              def useStateAt[U](eventId: TypedIdentifier)(f: EpochBoundaries[F] => F[U]): F[U] = {
                assert(eventId == nId)
                f(boundaryStore)
              }
            }
            consensusDataEventSourcedState = new EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[ConsensusDataEventSourcedState.ConsensusData[F]] = ???

              def useStateAt[U](
                eventId: TypedIdentifier
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
        bigBangId: TypedIdentifier,
        nId:       TypedIdentifier,
        address:   StakingAddresses.Operator
      ) =>
        withMock {
          val slot = 3
          for {
            boundaryStore <- TestStore.make[F, Epoch, TypedIdentifier]
            consensusData <- (
              TestStore.make[F, StakingAddresses.Operator, Int128],
              TestStore.make[F, Unit, Int128],
              TestStore.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- consensusData.operatorStakes.put(address, 1)
            _ <- consensusData.totalActiveStake.put((), 5)
            epochBoundaryEventSourcedState = new EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[EpochBoundaries[F]] = ???

              def useStateAt[U](eventId: TypedIdentifier)(f: EpochBoundaries[F] => F[U]): F[U] = {
                assert(eventId == nId)
                f(boundaryStore)
              }
            }
            consensusDataEventSourcedState = new EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[ConsensusDataEventSourcedState.ConsensusData[F]] = ???

              def useStateAt[U](
                eventId: TypedIdentifier
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
        bigBangId:    TypedIdentifier,
        nId:          TypedIdentifier,
        address:      StakingAddresses.Operator,
        registration: Box.Values.Registrations.Operator
      ) =>
        withMock {
          val slot = 3
          for {
            boundaryStore <- TestStore.make[F, Epoch, TypedIdentifier]
            consensusData <- (
              TestStore.make[F, StakingAddresses.Operator, Int128],
              TestStore.make[F, Unit, Int128],
              TestStore.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
            ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
            _ <- consensusData.registrations.put(address, registration)
            epochBoundaryEventSourcedState = new EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[EpochBoundaries[F]] = ???

              def useStateAt[U](eventId: TypedIdentifier)(f: EpochBoundaries[F] => F[U]): F[U] = {
                assert(eventId == nId)
                f(boundaryStore)
              }
            }
            consensusDataEventSourcedState = new EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
              F
            ], TypedIdentifier] {
              def stateAt(eventId: TypedIdentifier): F[ConsensusDataEventSourcedState.ConsensusData[F]] = ???

              def useStateAt[U](
                eventId: TypedIdentifier
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
