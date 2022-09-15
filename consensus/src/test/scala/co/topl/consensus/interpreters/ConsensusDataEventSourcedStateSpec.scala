package co.topl.consensus.interpreters

import cats.Applicative
import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.ListSet

class ConsensusDataEventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Retrieve the stake information for an operator at a particular block") {
    withMock {
      val address = arbitraryFullAddress.arbitrary.first.copy(
        stakingAddress = arbitraryOperatorStakingAddress.arbitrary.first
      )
      val nonStakingFullAddress = arbitraryFullAddress.arbitrary.first.copy(
        stakingAddress = StakingAddresses.NonStaking
      )
      val bigBangParentId = arbitraryTypedIdentifier.arbitrary.first
      val bigBangId = arbitraryTypedIdentifier.arbitrary.first
      val bigBangBlockTransaction =
        Transaction(
          Chain.empty,
          Chain(
            Transaction.Output(address, Box.Values.Arbit(5), minting = true)
          ),
          Transaction.Schedule(0, 0, Long.MaxValue),
          None
        )

      for {
        parentChildTree <- ParentChildTree.FromRef.make[F, TypedIdentifier]
        initialState <- (
          TestStore.make[F, StakingAddresses.Operator, Int128],
          TestStore.make[F, Unit, Int128],
          TestStore.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
        ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
        _                <- initialState.totalActiveStake.put((), 0)
        bodyStore        <- TestStore.make[F, TypedIdentifier, BlockBodyV2]
        transactionStore <- TestStore.make[F, TypedIdentifier, Transaction]
        fetchTransactionOutput = (boxId: Box.Id) =>
          transactionStore.getOrRaise(boxId.transactionId).map(_.outputs.get(boxId.transactionOutputIndex).get)
        underTest <- ConsensusDataEventSourcedState.make[F](
          bigBangParentId.pure[F],
          parentChildTree,
          _ => Applicative[F].unit,
          initialState.pure[F],
          bodyStore.getOrRaise,
          transactionStore.getOrRaise,
          fetchTransactionOutput
        )

        _ <- parentChildTree.associate(bigBangId, bigBangParentId)
        _ <- bodyStore.put(bigBangId, ListSet(bigBangBlockTransaction.id.asTypedBytes))
        _ <- transactionStore.put(bigBangBlockTransaction.id, bigBangBlockTransaction)

        // Start from 0 Arbits
        // Move 5 Arbits to the Operator

        _ <- underTest.useStateAt(bigBangId)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(5: Int128) >>
          state.operatorStakes
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(5: Int128)
        )

        // Now spend those 5 arbits from the Operator
        // And create 3 arbits for the Operator and 2 arbits for a non-operator
        transaction2 = Transaction(
          Chain(
            Transaction.Input(
              Box.Id(bigBangBlockTransaction.id, 0),
              Propositions.Contextual.HeightLock(0),
              Proofs.Undefined,
              Box.Values.Arbit(5)
            )
          ),
          Chain(
            Transaction.Output(address, Box.Values.Arbit(3), minting = false),
            Transaction.Output(nonStakingFullAddress, Box.Values.Arbit(2), minting = false)
          ),
          Transaction.Schedule(0, 0, Long.MaxValue),
          None
        )

        blockId2 = arbitraryTypedIdentifier.arbitrary.first
        _ <- parentChildTree.associate(blockId2, bigBangId)
        _ <- bodyStore.put(blockId2, ListSet(transaction2.id.asTypedBytes))
        _ <- transactionStore.put(transaction2.id, transaction2)

        _ <- underTest.useStateAt(blockId2)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(3: Int128) >>
          state.operatorStakes
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(3: Int128)
        )

        // Spend the 2 Arbits from the non-operator
        // And create 1 Arbit for the operator and 1 Arbit for the non-operator
        transaction3 = Transaction(
          Chain(
            Transaction.Input(
              Box.Id(transaction2.id, 1),
              Propositions.Contextual.HeightLock(0),
              Proofs.Undefined,
              Box.Values.Arbit(2)
            )
          ),
          Chain(
            Transaction.Output(address, Box.Values.Arbit(1), minting = false),
            Transaction.Output(nonStakingFullAddress, Box.Values.Arbit(1), minting = false)
          ),
          Transaction.Schedule(0, 0, Long.MaxValue),
          None
        )

        blockId3 = arbitraryTypedIdentifier.arbitrary.first
        _ <- parentChildTree.associate(blockId3, blockId2)
        _ <- bodyStore.put(blockId3, ListSet(transaction3.id.asTypedBytes))
        _ <- transactionStore.put(transaction3.id, transaction3)

        _ <- underTest.useStateAt(blockId3)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(4: Int128) >>
          state.operatorStakes
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(4: Int128)
        )
        // Double check that UnapplyBlock works
        _ <- underTest.useStateAt(blockId2)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(3: Int128) >>
          state.operatorStakes
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(3: Int128)
        )
        _ <- underTest.useStateAt(bigBangId)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(5: Int128) >>
          state.operatorStakes
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(5: Int128)
        )
      } yield ()
    }
  }

  test("Return the registration of an operator at a particular block") {
    withMock {
      val address = arbitraryFullAddress.arbitrary.first.copy(
        stakingAddress = arbitraryOperatorStakingAddress.arbitrary.first
      )
      val bigBangParentId = arbitraryTypedIdentifier.arbitrary.first
      val bigBangId = arbitraryTypedIdentifier.arbitrary.first
      val bigBangBlockTransaction =
        Transaction(
          Chain.empty,
          Chain(
            Transaction.Output(
              address,
              Box.Values.Registrations.Operator(arbitraryProofsKnowledgeKesProduct.arbitrary.first),
              minting = true
            )
          ),
          Transaction.Schedule(0, 0, Long.MaxValue),
          None
        )

      for {
        parentChildTree <- ParentChildTree.FromRef.make[F, TypedIdentifier]
        initialState <- (
          TestStore.make[F, StakingAddresses.Operator, Int128],
          TestStore.make[F, Unit, Int128],
          TestStore.make[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
        ).mapN((operatorStakes, totalActiveStake, registrations) =>
          ConsensusDataEventSourcedState.ConsensusData[F](operatorStakes, totalActiveStake, registrations)
        )
        _                <- initialState.totalActiveStake.put((), 0)
        bodyStore        <- TestStore.make[F, TypedIdentifier, BlockBodyV2]
        transactionStore <- TestStore.make[F, TypedIdentifier, Transaction]
        fetchTransactionOutput = (boxId: Box.Id) =>
          transactionStore.getOrRaise(boxId.transactionId).map(_.outputs.get(boxId.transactionOutputIndex).get)
        underTest <- ConsensusDataEventSourcedState.make[F](
          bigBangParentId.pure[F],
          parentChildTree,
          _ => Applicative[F].unit,
          initialState.pure[F],
          bodyStore.getOrRaise,
          transactionStore.getOrRaise,
          fetchTransactionOutput
        )

        _ <- parentChildTree.associate(bigBangId, bigBangParentId)
        _ <- bodyStore.put(bigBangId, ListSet(bigBangBlockTransaction.id.asTypedBytes))
        _ <- transactionStore.put(bigBangBlockTransaction.id, bigBangBlockTransaction)

        _ <- underTest.useStateAt(bigBangId)(state =>
          state.registrations
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(
              bigBangBlockTransaction.outputs.headOption.get.value.asInstanceOf[Box.Values.Registrations.Operator]
            )
        )

        transaction2 = Transaction(
          Chain(
            Transaction.Input(
              Box.Id(bigBangBlockTransaction.id, 0),
              Propositions.Contextual.HeightLock(0),
              Proofs.Undefined,
              bigBangBlockTransaction.outputs.headOption.get.value
            )
          ),
          Chain.empty,
          Transaction.Schedule(0, 0, Long.MaxValue),
          None
        )

        blockId2 = arbitraryTypedIdentifier.arbitrary.first
        _ <- parentChildTree.associate(blockId2, bigBangId)
        _ <- bodyStore.put(blockId2, ListSet(transaction2.id.asTypedBytes))
        _ <- transactionStore.put(transaction2.id, transaction2)

        _ <- underTest.useStateAt(blockId2)(state =>
          state.registrations.get(address.stakingAddress.asInstanceOf[StakingAddresses.Operator]).assertEquals(None)
        )

        // Double check that UnapplyBlock works
        _ <- underTest.useStateAt(bigBangId)(state =>
          state.registrations
            .getOrRaise(address.stakingAddress.asInstanceOf[StakingAddresses.Operator])
            .assertEquals(
              bigBangBlockTransaction.outputs.headOption.get.value.asInstanceOf[Box.Values.Registrations.Operator]
            )
        )
      } yield ()
    }
  }
}
