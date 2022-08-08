package co.topl.consensus.interpreters

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.testInterpreters.TestStore
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class EpochBoundariesEventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Retrieve an epoch boundary block ID for a positive/existent epoch") {
    withMock {
      val slotData: List[SlotData] =
        List.unfold(
          Chain.empty[SlotData]
        ) { slotData =>
          val _next = arbitrarySlotData.arbitrary.first
          val next =
            slotData.lastOption.fold(
              _next.copy(slotId = _next.slotId.copy(slot = 0))
            )(last =>
              _next.copy(
                slotId = _next.slotId.copy(slot = last.slotId.slot + 1),
                parentSlotId = last.slotId
              )
            )
          if (slotData.length < 10) slotData.append(next).some.tupleLeft(next)
          else none
        }

      for {
        clock <- mock[ClockAlgebra[F]].pure[F]
        currentBlockId = slotData.head.parentSlotId.blockId
        parentChildTree <- ParentChildTree.FromSemaphore.make[F, TypedIdentifier]
        initialState    <- TestStore.make[F, Epoch, TypedIdentifier]
        fetchSlotData = (id: TypedIdentifier) => slotData.find(_.slotId.blockId === id).get.pure[F]

        _ <- slotData.traverse(slotDatum =>
          parentChildTree.associate(slotDatum.slotId.blockId, slotDatum.parentSlotId.blockId)
        )

        _ = (() => clock.slotsPerEpoch).expects().anyNumberOfTimes().returning(2L.pure[F])

        underTest <- EpochBoundariesEventSourcedState
          .make[F](clock, currentBlockId.pure[F], parentChildTree, initialState.pure[F], fetchSlotData)

        _ <- underTest.useStateAt(slotData.last.slotId.blockId)(state =>
          state.getOrRaise(0).assertEquals(slotData(1).slotId.blockId) >>
          state.getOrRaise(1).assertEquals(slotData(3).slotId.blockId) >>
          state.getOrRaise(2).assertEquals(slotData(5).slotId.blockId) >>
          state.getOrRaise(3).assertEquals(slotData(7).slotId.blockId) >>
          state.getOrRaise(4).assertEquals(slotData(9).slotId.blockId)
        )

      } yield ()
    }
  }

  test("Return None for a negative or non-existent epoch") {
    withMock {
      val slotData: List[SlotData] =
        List(arbitrarySlotData.arbitrary.first).map(d => d.copy(slotId = d.slotId.copy(slot = 0)))

      for {
        clock <- mock[ClockAlgebra[F]].pure[F]
        currentBlockId = slotData.head.parentSlotId.blockId
        parentChildTree <- ParentChildTree.FromSemaphore.make[F, TypedIdentifier]
        initialState    <- TestStore.make[F, Epoch, TypedIdentifier]
        fetchSlotData = (id: TypedIdentifier) => slotData.find(_.slotId.blockId === id).get.pure[F]

        _ <- slotData.traverse(slotDatum =>
          parentChildTree.associate(slotDatum.slotId.blockId, slotDatum.parentSlotId.blockId)
        )

        _ = (() => clock.slotsPerEpoch).expects().anyNumberOfTimes().returning(2L.pure[F])

        underTest <- EpochBoundariesEventSourcedState
          .make[F](clock, currentBlockId.pure[F], parentChildTree, initialState.pure[F], fetchSlotData)

        _ <- underTest.useStateAt(slotData.last.slotId.blockId)(state =>
          state.get(-1).assertEquals(None) >>
          state.get(2).assertEquals(None)
        )

      } yield ()
    }
  }
}
