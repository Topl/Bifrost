package co.topl.consensus.interpreters

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.consensus.models._
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class ConsensusDataEventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  val lock: Lock = Lock().withPredicate(Lock.Predicate())

  val lockAddress: LockAddress = lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)

  test("Retrieve the stake information for an operator at a particular block") {
    withMock {
      val bigBangParentId = arbitraryBlockId.arbitrary.first
      val bigBangId = arbitraryBlockId.arbitrary.first
      val registration = arbitraryStakingRegistration.arbitrary.first
      val bigBangBlockTransaction =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withOutputs(
            List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, registration.some))))
          )
          .embedId

      for {
        parentChildTree <- ParentChildTree.FromRef.make[F, BlockId]
        initialState <- (
          TestStore.make[F, Unit, BigInt],
          TestStore.make[F, Unit, BigInt],
          TestStore.make[F, StakingAddress, ActiveStaker]
        ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
        _                <- initialState.totalActiveStake.put((), 0)
        _                <- initialState.totalInactiveStake.put((), 0)
        bodyStore        <- TestStore.make[F, BlockId, BlockBody]
        transactionStore <- TestStore.make[F, TransactionId, IoTransaction]
        underTest <- ConsensusDataEventSourcedState.make[F](
          bigBangParentId.pure[F],
          parentChildTree,
          _ => Applicative[F].unit,
          initialState.pure[F],
          bodyStore.getOrRaise,
          transactionStore.getOrRaise
        )

        _ <- parentChildTree.associate(bigBangId, bigBangParentId)
        _ <- bodyStore.put(bigBangId, BlockBody(List(bigBangBlockTransaction.id)))
        _ <- transactionStore.put(bigBangBlockTransaction.id, bigBangBlockTransaction)

        // Start from 0 Topls (pre-big-bang)

        // Move 5 Topls to the Operator
        _ <- underTest.useStateAt(bigBangId)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(5: BigInt) >>
          state.totalInactiveStake.getOrRaise(()).assertEquals(0: BigInt) >>
          state.stakers.get(registration.address).assertEquals(ActiveStaker(registration, 5).some)
        )

        // Now spend those 5 Topls from the Operator
        // And create 3 Topls for the Operator and 2 Topls for a non-operator
        transaction2 = IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(
              SpentTransactionOutput(
                bigBangBlockTransaction.id.outputAddress(0, 0, 0),
                Attestation().withPredicate(Attestation.Predicate.defaultInstance),
                bigBangBlockTransaction.outputs(0).value
              )
            )
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(4, registration.some))),
              UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, none)))
            )
          )
        transaction3 = IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(
              SpentTransactionOutput(
                transaction2.id.outputAddress(0, 0, 0),
                Attestation().withPredicate(Attestation.Predicate.defaultInstance),
                transaction2.outputs(0).value
              )
            )
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(3, registration.some))),
              UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, none)))
            )
          )

        blockId2 = arbitraryBlockId.arbitrary.first
        _ <- parentChildTree.associate(blockId2, bigBangId)
        _ <- bodyStore.put(
          blockId2,
          BlockBody(List(transaction2.id, transaction3.id))
        )
        _ <- transactionStore.put(transaction2.id, transaction2)
        _ <- transactionStore.put(transaction3.id, transaction3)

        _ <- underTest.useStateAt(blockId2)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(3: BigInt) >>
          state.totalInactiveStake.getOrRaise(()).assertEquals(2: BigInt) >>
          state.stakers.get(registration.address).assertEquals(ActiveStaker(registration, 3).some)
        )

        // Spend the 2 Topls from the non-operator and 3 Topls from the operator
        // And create 4 Topl for the operator and 1 Topl for the non-operator
        transaction4 = IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withInputs(
            List(
              SpentTransactionOutput(
                transaction2.id.outputAddress(0, 0, 1),
                Attestation().withPredicate(Attestation.Predicate.defaultInstance),
                transaction2.outputs(1).value
              ),
              SpentTransactionOutput(
                transaction3.id.outputAddress(0, 0, 1),
                Attestation().withPredicate(Attestation.Predicate.defaultInstance),
                transaction3.outputs(1).value
              ),
              SpentTransactionOutput(
                transaction3.id.outputAddress(0, 0, 0),
                Attestation().withPredicate(Attestation.Predicate.defaultInstance),
                transaction3.outputs(0).value
              )
            )
          )
          .withOutputs(
            List(
              UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(4, registration.some))),
              UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, none)))
            )
          )

        blockId3 = arbitraryBlockId.arbitrary.first
        _ <- parentChildTree.associate(blockId3, blockId2)
        _ <- bodyStore.put(
          blockId3,
          BlockBody(List(transaction4.id))
        )
        _ <- transactionStore.put(transaction4.id, transaction4)

        _ <- underTest.useStateAt(blockId3)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(4: BigInt) >>
          state.totalInactiveStake.getOrRaise(()).assertEquals(1: BigInt) >>
          state.stakers.get(registration.address).assertEquals(ActiveStaker(registration, 4).some)
        )
        // Double check that UnapplyBlock works
        _ <- underTest.useStateAt(blockId2)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(3: BigInt) >>
          state.totalInactiveStake.getOrRaise(()).assertEquals(2: BigInt) >>
          state.stakers.get(registration.address).assertEquals(ActiveStaker(registration, 3).some)
        )
        _ <- underTest.useStateAt(bigBangId)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(5: BigInt) >>
          state.totalInactiveStake.getOrRaise(()).assertEquals(0: BigInt) >>
          state.stakers.get(registration.address).assertEquals(ActiveStaker(registration, 5).some)
        )
      } yield ()
    }
  }

}
