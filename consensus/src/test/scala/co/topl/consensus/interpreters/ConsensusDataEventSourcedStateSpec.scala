package co.topl.consensus.interpreters

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.models._
import co.topl.brambl.models.box._
import co.topl.brambl.models.transaction._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SignatureKesProduct
import co.topl.eventtree.ParentChildTree
import co.topl.node.models.BlockBody
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.ModelGenerators._
import co.topl.models.StakingAddress
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import quivr.models.SmallData

class ConsensusDataEventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  val defaultDatum =
    Datum.IoTransaction(
      Event.IoTransaction(
        Schedule(0, Long.MaxValue, Long.MaxValue),
        SmallData.defaultInstance
      )
    )
  val lock: Lock = ???
  val lockAddress: LockAddress = ???

  test("Retrieve the stake information for an operator at a particular block") {
    withMock {
      val stakingAddress = stakingAddressGen.first
      val bigBangParentId = arbitraryBlockId.arbitrary.first
      val bigBangId = arbitraryBlockId.arbitrary.first
      val bigBangBlockTransaction =
        IoTransaction(
          Nil,
          List(UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(5, stakingAddress)))),
          defaultDatum
        )

      for {
        parentChildTree <- ParentChildTree.FromRef.make[F, BlockId]
        initialState <- (
          TestStore.make[F, StakingAddress, BigInt],
          TestStore.make[F, Unit, BigInt],
          TestStore.make[F, StakingAddress, SignatureKesProduct]
        ).mapN(ConsensusDataEventSourcedState.ConsensusData[F])
        _                <- initialState.totalActiveStake.put((), 0)
        bodyStore        <- TestStore.make[F, BlockId, BlockBody]
        transactionStore <- TestStore.make[F, Identifier.IoTransaction32, IoTransaction]
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

        // Start from 0 Arbits
        // Move 5 Arbits to the Operator

        _ <- underTest.useStateAt(bigBangId)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(5: BigInt) >>
          state.operatorStakes.getOrRaise(stakingAddress).assertEquals(5: BigInt)
        )

        // Now spend those 5 arbits from the Operator
        // And create 3 arbits for the Operator and 2 arbits for a non-operator
        transaction2 = IoTransaction(
          List(
            SpentTransactionOutput(
              txOutputAddressFrom(bigBangBlockTransaction.id, 0),
              Attestation.defaultInstance,
              bigBangBlockTransaction.outputs(0).value
            )
          ),
          List(
            UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(4, stakingAddress))),
            UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, ByteString.EMPTY)))
          ),
          defaultDatum
        )
        transaction3 = IoTransaction(
          List(
            SpentTransactionOutput(
              txOutputAddressFrom(transaction2.id, 0),
              Attestation.defaultInstance,
              transaction2.outputs(0).value
            )
          ),
          List(
            UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(3, stakingAddress))),
            UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, ByteString.EMPTY)))
          ),
          defaultDatum
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
          state.operatorStakes.getOrRaise(stakingAddress).assertEquals(3: BigInt)
        )

        // Spend the 2 Arbits from the non-operator
        // And create 1 Arbit for the operator and 1 Arbit for the non-operator
        transaction4 = IoTransaction(
          List(
            SpentTransactionOutput(
              txOutputAddressFrom(transaction2.id, 1),
              Attestation.defaultInstance,
              transaction2.outputs(1).value
            ),
            SpentTransactionOutput(
              txOutputAddressFrom(transaction3.id, 1),
              Attestation.defaultInstance,
              transaction3.outputs(1).value
            )
          ),
          List(
            UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, stakingAddress))),
            UnspentTransactionOutput(lockAddress, Value().withTopl(Value.TOPL(1, ByteString.EMPTY)))
          ),
          defaultDatum
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
          state.operatorStakes.getOrRaise(stakingAddress).assertEquals(4: BigInt)
        )
        // Double check that UnapplyBlock works
        _ <- underTest.useStateAt(blockId2)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(3: BigInt) >>
          state.operatorStakes.getOrRaise(stakingAddress).assertEquals(3: BigInt)
        )
        _ <- underTest.useStateAt(bigBangId)(state =>
          state.totalActiveStake.getOrRaise(()).assertEquals(5: BigInt) >>
          state.operatorStakes.getOrRaise(stakingAddress).assertEquals(5: BigInt)
        )
      } yield ()
    }
  }

  test("Return the registration of an operator at a particular block") {
    withMock {
      val stakingAddress = stakingAddressGen.first
      val bigBangParentId = arbitraryBlockId.arbitrary.first
      val bigBangId = arbitraryBlockId.arbitrary.first
      val bigBangBlockTransaction =
        IoTransaction(
          Nil,
          List(
            UnspentTransactionOutput(lockAddress, Value().withRegistration(Value.Registration(???, stakingAddress)))
          ),
          defaultDatum
        )

      for {
        parentChildTree <- ParentChildTree.FromRef.make[F, BlockId]
        initialState <- (
          TestStore.make[F, StakingAddress, BigInt],
          TestStore.make[F, Unit, BigInt],
          TestStore.make[F, StakingAddress, SignatureKesProduct]
        ).mapN((operatorStakes, totalActiveStake, registrations) =>
          ConsensusDataEventSourcedState.ConsensusData[F](operatorStakes, totalActiveStake, registrations)
        )
        _                <- initialState.totalActiveStake.put((), 0)
        bodyStore        <- TestStore.make[F, BlockId, BlockBody]
        transactionStore <- TestStore.make[F, Identifier.IoTransaction32, IoTransaction]
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

        _ <- underTest.useStateAt(bigBangId)(state =>
          state.registrations
            .getOrRaise(stakingAddress)
            .assertEquals(
              bigBangBlockTransaction.outputs.headOption.get.value.getRegistration.registration
            )
        )

        transaction2 = IoTransaction(
          List(
            SpentTransactionOutput(
              txOutputAddressFrom(bigBangBlockTransaction.id, 0),
              Attestation.defaultInstance,
              bigBangBlockTransaction.outputs(0).value
            )
          ),
          Nil,
          defaultDatum
        )

        blockId2 = arbitraryBlockId.arbitrary.first
        _ <- parentChildTree.associate(blockId2, bigBangId)
        _ <- bodyStore.put(
          blockId2,
          BlockBody(List(transaction2.id))
        )
        _ <- transactionStore.put(transaction2.id, transaction2)

        _ <- underTest.useStateAt(blockId2)(state => state.registrations.get(stakingAddress).assertEquals(None))

        // Double check that UnapplyBlock works
        _ <- underTest.useStateAt(bigBangId)(state =>
          state.registrations
            .getOrRaise(stakingAddress)
            .assertEquals(
              bigBangBlockTransaction.outputs(0).value.getRegistration.registration
            )
        )
      } yield ()
    }
  }

  private def txOutputAddressFrom(id: Identifier.IoTransaction32, index: Int) =
    TransactionOutputAddress(
      0,
      0,
      index,
      TransactionOutputAddress.Id.IoTransaction32(id)
    )

}
