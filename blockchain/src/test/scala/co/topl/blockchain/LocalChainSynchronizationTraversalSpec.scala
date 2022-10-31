package co.topl.blockchain

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.SynchronizationTraversalSteps.{Applied, Unapplied}
import co.topl.eventtree.ParentChildTree
import co.topl.models.ModelGenerators._
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

class LocalChainSynchronizationTraversalSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("Canonical Head Steps Block IDs should be produced in a stream whenever they are adopted locally") {

    val slot_A = arbitrarySlotData.arbitrary.first
    val slot_B = arbitrarySlotData.arbitrary.first
    val slot_C = arbitrarySlotData.arbitrary.first
    val slot_D = arbitrarySlotData.arbitrary.first
    val slot_E = arbitrarySlotData.arbitrary.first
    val slot_F = arbitrarySlotData.arbitrary.first

    for {
      parentChildTree <- ParentChildTree.FromRef.make[F, TypedIdentifier]

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

      adoptions = Source(Seq(slot_C, slot_F).map(_.slotId.blockId))

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

  implicit val system: ActorSystem = ActorSystem("LocalChainHeadTraversalSpec")

  override def afterAll(): Unit =
    super.afterAll()
}
