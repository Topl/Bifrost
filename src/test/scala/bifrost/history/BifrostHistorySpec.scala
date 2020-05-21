package bifrost.history

import bifrost.BifrostGenerators
import bifrost.NodeViewModifier.ModifierId
import bifrost.modifier.block.Block
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.encode.Base58

class BifrostHistorySpec extends PropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with BifrostGenerators {

  var history: BifrostHistory = generateHistory

  property("Block application should result in storage and BifrostHistory.continuationIds") {
    var ids: Seq[ModifierId] = Seq()

    /* Apply blocks and ensure that they are stored */
    forAll(BlockGen) { blockTemp =>

      val block = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1

      history.modifierById(block.id).isDefined shouldBe true
      ids = ids :+ block.id
    }

    val startFrom = ids.head

    /* Continuation ids should get the block start up to the end of the chain */
    val continuationIds = history
      .continuationIds(Seq((Block.ModifierTypeId, startFrom)), ids.length)
      .get
      .map(_._2)

    continuationIds.map(Base58.encode) shouldEqual ids.map(Base58.encode)


    forAll(Gen.choose(0, ids.length - 1)) { startIndex: Int =>
      val startFrom = Seq((Block.ModifierTypeId, ids(startIndex)))
      val startList = ids.take(startIndex + 1).map((Block.ModifierTypeId, _))
      val restIds = ids.zipWithIndex.filter {
        case (_, index) => index >= startIndex
      }.map(_._1).map(Base58.encode)

      val continuationIds = history.continuationIds(startFrom, ids.length).get.map(_._2)

      /* Starting from a random block should give us continuation ids of the chain from that block */
      continuationIds.map(Base58.encode) shouldEqual restIds

      val limit = 5
      val continuation = history.continuationIds(startList, limit).get

      continuation.length shouldBe Math.min(limit, restIds.length)

      startList.exists(sl => sl._2 sameElements continuation.head._2) shouldBe true

      continuation.tail.foreach { c =>
        startList.exists(sl => sl._2 sameElements c._2) shouldBe false
      }
    }
  }

}
