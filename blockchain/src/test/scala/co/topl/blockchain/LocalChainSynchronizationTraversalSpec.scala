package co.topl.blockchain

import cats.effect.IO
import co.topl.algebras.SynchronizationTraversalSteps.{Applied, Unapplied}
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.consensus.models.SlotData
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import fs2.{Chunk, Stream}

class LocalChainSynchronizationTraversalSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("Canonical Head Steps Block IDs should be produced in a stream whenever they are adopted locally") {

    PropF.forAllF {
      (slot_A: SlotData, slot_B: SlotData, slot_C: SlotData, slot_D: SlotData, slot_E: SlotData, slot_F: SlotData) =>
        for {
          parentChildTree <- ParentChildTree.FromRef.make[F, BlockId]

          /**
           * Parent Tree
           * A -> B -> C
           * _ -> D -> E -> F
           */

          _ <- parentChildTree.associate(slot_B.slotId.blockId, slot_A.slotId.blockId)
          _ <- parentChildTree.associate(slot_C.slotId.blockId, slot_B.slotId.blockId)

          _ <- parentChildTree.associate(slot_D.slotId.blockId, slot_A.slotId.blockId)
          _ <- parentChildTree.associate(slot_E.slotId.blockId, slot_D.slotId.blockId)
          _ <- parentChildTree.associate(slot_F.slotId.blockId, slot_E.slotId.blockId)

          adoptions = Stream.chunk(
            Chunk(slot_C.slotId.blockId: BlockId, slot_F.slotId.blockId: BlockId)
          )

          stream <- LocalChainSynchronizationTraversal
            .make[F](slot_A.slotId.blockId, adoptions, parentChildTree)
            .headChanges

          expected = List(
            Applied(slot_B.slotId.blockId),
            Applied(slot_C.slotId.blockId),
            Unapplied(slot_B.slotId.blockId),
            Unapplied(slot_C.slotId.blockId),
            Applied(slot_D.slotId.blockId),
            Applied(slot_E.slotId.blockId),
            Applied(slot_F.slotId.blockId)
          )
          _ <- stream.compile.toList.map(_ == expected).assert

        } yield ()
    }
  }

  override def afterAll(): Unit =
    super.afterAll()
}
