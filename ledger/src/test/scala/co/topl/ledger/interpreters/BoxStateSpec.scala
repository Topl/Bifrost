package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptySet}
import cats.effect.IO
import co.topl.models.{Box, Transaction, TypedIdentifier}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import co.topl.models.ModelGenerators._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.typeclasses.implicits._
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.eventtree.ParentChildTree

import scala.collection.immutable.ListSet

class BoxStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  test("BoxState includes new outputs") {
    PropF.forAllF {
      (
        blockId0:         TypedIdentifier,
        transaction1Base: Transaction,
        output:           Transaction.Output,
        blockId1:         TypedIdentifier,
        transaction2Base: Transaction,
        input:            Transaction.Input,
        blockId2:         TypedIdentifier
      ) =>
        val transaction1 =
          transaction1Base.copy(inputs = Chain.empty, outputs = transaction1Base.outputs.append(output))
        val outputBoxId =
          Box.Id(transaction1.id, (transaction1.outputs.length - 1).toShort)
        val transaction2 = transaction2Base.copy(inputs = Chain(input.copy(boxId = outputBoxId)))

        for {
          parentChildTree <- ParentChildTree.FromRef.make[IO, TypedIdentifier]
          _               <- parentChildTree.associate(blockId1, blockId0)
          _               <- parentChildTree.associate(blockId2, blockId1)
          underTest <- BoxState.make[IO](
            blockId0.pure[IO],
            Map(
              blockId1 -> ListSet(transaction1.id.asTypedBytes).pure[IO],
              blockId2 -> ListSet(transaction2.id.asTypedBytes).pure[IO]
            ).apply _,
            Map(
              transaction1.id.asTypedBytes -> transaction1.pure[IO],
              transaction2.id.asTypedBytes -> transaction2.pure[IO]
            ),
            parentChildTree,
            _ => IO.unit,
            TestStore.make[IO, TypedIdentifier, NonEmptySet[Short]].widen
          )

          _ <- underTest.boxExistsAt(blockId1)(outputBoxId).assert
          _ <- underTest.boxExistsAt(blockId2)(outputBoxId).map(!_).assert
        } yield ()
    }
  }
}
