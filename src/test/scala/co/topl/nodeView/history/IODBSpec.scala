package co.topl.nodeView.history

import java.io.File

import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.{CoreGenerators, ValidGenerators}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }

import scala.util.Random
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.Random

class IODBSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {


  val iFile = new File(s"/tmp/bifrost/scorextest-${Random.nextInt(10000000)}")
  iFile.mkdirs()
  val blocksStorage = new LSMStore(iFile)
  blocksStorage.update(ByteArrayWrapper(Array[Byte](1)), Seq(), Seq())

  property("Rollback should not touch keys before") {

    /**
      * Apply a transaction by storing its new boxes (ignore old boxes)
      *
      * @param tx the transaction to write boxes to storage
      */
    def writeTx(tx: Transaction): Unit = {
      val boxIdsToRemove: Iterable[ByteArrayWrapper] = Seq()
      val boxesToAdd: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
        tx.newBoxes
          .map(b => (ByteArrayWrapper(b.id.hashBytes), ByteArrayWrapper(b.bytes)))
          .toList

      blocksStorage.update(ByteArrayWrapper(tx.id.hashBytes), boxIdsToRemove, boxesToAdd)
    }

    /**
      * Check that the boxes for the transaction are all stored
      *
      * @param tx the transaction to check has boxes in storage
      */
    def checkTx(tx: Transaction): Unit = {
      tx.newBoxes
        .foreach(b => require(blocksStorage.get(ByteArrayWrapper(b.id.hashBytes)).isDefined))
    }

    forAll(validBifrostTransactionSeqGen) { txs =>
      whenever(txs.length >= 2) {
        blocksStorage.rollback(ByteArrayWrapper(Array[Byte](1)))

        /* Make sure transactions get written to storage */
        txs.foreach(tx => {
          writeTx(tx)
          checkTx(tx)
        })

        val head = txs.head

        /* Rollback to head shouldn't affect the head tx */
        blocksStorage.rollback(ByteArrayWrapper(head.id.hashBytes))
        checkTx(head)
      }
    }
  }

  property("Writing a block should result in storage of block") {

    /**
      * Apply a block by storing all of its transactions' new boxes (ignore old boxes)
      *
      * @param b the block to write tx boxes to storage
      */

    def writeBlock(b: Block): Unit = {
      blocksStorage.update(
        ByteArrayWrapper(b.id.hashBytes),
        Seq(),
        Seq(ByteArrayWrapper(b.id.hashBytes) -> ByteArrayWrapper(Block.modifierTypeId +: b.bytes))
      )
    }

    var ids: Seq[ModifierId] = Seq()

    forAll(BlockGen) { block =>
      ids = block.id +: ids
      writeBlock(block)
      blocksStorage.get(ByteArrayWrapper(block.id.hashBytes)).isDefined shouldBe true
    }

    ids.foreach {
      id => {
        val idInStorage = blocksStorage.get(ByteArrayWrapper(id.hashBytes)) match {
          case None => println(s"${Console.RED} Id ${id.toString} not found"); false
          case Some(_) => true
        }
        require(idInStorage)
      }
    }
  }

}
