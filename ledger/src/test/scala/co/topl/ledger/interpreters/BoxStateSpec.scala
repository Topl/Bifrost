package co.topl.ledger.interpreters

import cats.data.NonEmptySet
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models._
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF

class BoxStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  test("BoxState includes new outputs") {
    PropF.forAllF {
      (
        blockId0: BlockId,
        output:   UnspentTransactionOutput,
        blockId1: BlockId,
        input:    SpentTransactionOutput,
        blockId2: BlockId
      ) =>
        val transaction1 = IoTransaction.defaultInstance.withOutputs(List(output))
        val outputBoxId = transaction1.id.outputAddress(0, 0, transaction1.outputs.length - 1)

        val transaction2 = IoTransaction.defaultInstance.withInputs(List(input.copy(address = outputBoxId)))

        for {
          parentChildTree <- ParentChildTree.FromRef.make[IO, BlockId]
          _               <- parentChildTree.associate(blockId1, blockId0)
          _               <- parentChildTree.associate(blockId2, blockId1)
          (underTest, _) <- BoxState.make[IO](
            blockId0.pure[IO],
            Map(
              blockId1 -> BlockBody(List(transaction1.id)).pure[IO],
              blockId2 -> BlockBody(List(transaction2.id)).pure[IO]
            ).apply _,
            Map(
              transaction1.id -> transaction1.pure[IO],
              transaction2.id -> transaction2.pure[IO]
            ),
            parentChildTree,
            _ => IO.unit,
            TestStore.make[IO, TransactionId, NonEmptySet[Short]].widen
          )
          _ <- underTest.boxExistsAt(blockId1)(outputBoxId).assert
          _ <- underTest.boxExistsAt(blockId2)(outputBoxId).map(!_).assert
        } yield ()
    }
  }
}
