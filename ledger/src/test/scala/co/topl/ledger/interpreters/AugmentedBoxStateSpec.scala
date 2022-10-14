package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptySet}
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.{Box, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF

import scala.collection.immutable.ListSet

class AugmentedBoxStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  test("BoxState includes new outputs and exclude spent outputs") {
    PropF.forAllF {
      (
        blockId0:         TypedIdentifier,
        txBase1:          Transaction,
        txOutput10:       Transaction.Output,
        txOutput11:       Transaction.Output,
        transaction2Base: Transaction,
        input:            Transaction.Input,
        blockId1:         TypedIdentifier
      ) =>
        val transaction1 =
          txBase1.copy(inputs = Chain.empty, outputs = txBase1.outputs ++ Chain(txOutput10, txOutput11))
        val outputBoxId10 = Box.Id(transaction1.id, (transaction1.outputs.length - 2).toShort)
        val outputBoxId11 = Box.Id(transaction1.id, (transaction1.outputs.length - 1).toShort)
        val transaction2 = transaction2Base.copy(inputs = Chain(input.copy(boxId = outputBoxId11)))

        for {
          parentChildTree <- ParentChildTree.FromRef.make[IO, TypedIdentifier]
          boxState <- BoxState.make[IO](
            blockId0.pure[IO],
            Map(
              blockId1 -> ListSet(transaction1.id.asTypedBytes, transaction2.id.asTypedBytes).pure[IO]
            ).apply _,
            Map(
              transaction1.id.asTypedBytes -> transaction1.pure[IO],
              transaction2.id.asTypedBytes -> transaction2.pure[IO]
            ),
            parentChildTree,
            _ => IO.unit,
            TestStore.make[IO, TypedIdentifier, NonEmptySet[Short]].widen
          )

          stateAugmentation <-
            AugmentedBoxState.StateAugmentation.empty.augment(transaction1).augment(transaction2).pure[IO]

          underTest <- AugmentedBoxState.make(boxState)(stateAugmentation)

          _ <- underTest.boxExistsAt(blockId1)(outputBoxId10).assert
          _ <- underTest.boxExistsAt(blockId1)(outputBoxId11).map(!_).assert
        } yield ()
    }
  }
}
