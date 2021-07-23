package co.topl.nodeView.history

import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.utils.NodeGenerators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BifrostHistorySpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with NodeGenerators {

  var history: History = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    history = generateHistory()
  }

  property("Block application should result in storage and BifrostHistory.continuationIds") {
    var ids: Seq[ModifierId] = Seq()

    /* Apply blocks and ensure that they are stored */
    forAll(blockCurve25519Gen) { blockTemp =>
      val block = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1

      history.modifierById(block.id).isDefined shouldBe true
      ids = ids :+ block.id
    }

    val startFrom = ids.head

    /* Continuation ids should get the block start up to the end of the chain */
    val continuationIds = history
      .continuationIds(Seq((Block.modifierTypeId, startFrom)), ids.length)
      .get
      .map(_._2)

    continuationIds shouldEqual ids

    forAll(Gen.choose(0, ids.length - 1)) { startIndex: Int =>
      val startFrom = Seq((Block.modifierTypeId, ids(startIndex)))
      val startList = ids.take(startIndex + 1).map((Block.modifierTypeId, _))
      val restIds = ids.zipWithIndex
        .filter { case (_, index) =>
          index >= startIndex
        }
        .map(_._1)

      val continuationIds = history.continuationIds(startFrom, ids.length).get.map(_._2)

      /* Starting from a random block should give us continuation ids of the chain from that block */
      continuationIds shouldEqual restIds

      val limit = 5
      val continuation = history.continuationIds(startList, limit).get

      continuation.length shouldBe Math.min(limit, restIds.length)

      startList.exists(sl => sl._2 == continuation.head._2) shouldBe true

      continuation.tail.foreach { c =>
        startList.exists(sl => sl._2 == c._2) shouldBe false
      }
    }
  }

}
