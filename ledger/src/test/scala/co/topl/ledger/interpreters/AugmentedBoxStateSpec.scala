package co.topl.ledger.interpreters

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.models._
import co.topl.brambl.models.transaction._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF

class AugmentedBoxStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  test("BoxState includes new outputs and exclude spent outputs") {
    PropF.forAllF {
      (
        blockId0:         BlockId,
        txBase1:          IoTransaction,
        txOutput10:       UnspentTransactionOutput,
        txOutput11:       UnspentTransactionOutput,
        transaction2Base: IoTransaction,
        input:            SpentTransactionOutput,
        blockId1:         BlockId
      ) =>
        val transaction1 = txBase1.addOutputs(txOutput10, txOutput11)
        val outputBoxId10 = transaction1.id.outputAddress(0, 0, transaction1.outputs.length - 2)
        val outputBoxId11 = transaction1.id.outputAddress(0, 0, transaction1.outputs.length - 1)
        val transaction2 = transaction2Base.addInputs(input.copy(address = outputBoxId11))

        for {
          parentChildTree <- ParentChildTree.FromRef.make[IO, BlockId]
          boxState <- BoxState.make[IO](
            blockId0.pure[IO],
            Map(
              blockId1 ->
              BlockBody(List(transaction1.id, transaction2.id))
                .pure[IO]
            ).apply(_),
            Map(
              transaction1.id -> transaction1.pure[IO],
              transaction2.id -> transaction2.pure[IO]
            ),
            parentChildTree,
            _ => IO.unit,
            TestStore.make[IO, TransactionId, NonEmptySet[Short]].widen
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
