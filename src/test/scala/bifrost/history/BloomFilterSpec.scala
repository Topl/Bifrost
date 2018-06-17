package bifrost.history

/**
  * Created by cykoz on 7/11/2017.
  */

import bifrost.blocks.{BifrostBlock, Bloom}
import bifrost.transaction.ContractCompletion
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.collection.BitSet

class BloomFilterSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  var history: BifrostHistory = generateHistory

  property("Verify Bloom Calculation is correct") {
    val set = Bloom.calcBloom(Array.fill(32)(1), IndexedSeq(Array.fill(32)(1)))
    set shouldEqual BitSet(10, 138, 201)
  }

  property("Bloom filter should get a seq of contractCompletion transactions") {
    var completionTxs: Seq[ContractCompletion] = Seq()

    forAll(validBifrostTransactionSeqGen) { txs =>
      val block = BifrostBlock(history.bestBlockId,
                               System.currentTimeMillis(),
                               arbitBoxGen.sample.get,
                               signatureGen.sample.get,
                               txs)

      history = history.append(block).get._1

      txs.foreach {
        case a: ContractCompletion =>
          completionTxs = completionTxs :+ a
        case _ =>
      }

      history.modifierById(block.id).isDefined shouldBe true
    }

    completionTxs.foreach { tx =>
      val txs = history.bloomFilter(tx.bloomTopics.get)
      txs.length shouldBe 1
      txs.head.bytes sameElements tx.bytes shouldBe true
    }

    completionTxs.foreach { tx =>
      val txs = history.bloomFilter(IndexedSeq(tx.bloomTopics.get(2)))
      txs.length shouldBe 1
      txs.head.bytes sameElements tx.bytes shouldBe true
    }
  }
}