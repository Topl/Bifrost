package co.topl.modifier

import co.topl.modifier.block.{BloomFilter, TransactionsCarryingPersistentNodeViewModifier}
import co.topl.modifier.transaction.Transaction
import co.topl.utils.ValidGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class BloomFilterSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with ValidGenerators
    with Matchers {

  property("Bloomfilter should be able to tell if it definitely contains an address") {
    forAll(validBifrostTransactionSeqGen) { txs =>
      val bloomfilter: BloomFilter = TransactionsCarryingPersistentNodeViewModifier.createBloom(txs)

      txs.foreach { tx =>
        tx.bloomTopics.foreach { bt =>
          bloomfilter.contains(bt) shouldBe true
        }
      }
    }
  }

  property("Bloomfilter should be able to tell if an address is likely not in the block") {
    forAll(validBifrostTransactionSeqGen) { txs =>
      val bloomfilter: BloomFilter = TransactionsCarryingPersistentNodeViewModifier.createBloom(txs.dropRight(1))
      val addressCount = txs.foldLeft(0)(_+_.bloomTopics.size)

      val falsePositives = txs.last.bloomTopics.foldLeft(0) { (count, bt) =>
        if (bloomfilter.contains(bt)) count + 1
        else count
      }

      (falsePositives <= addressCount/100) shouldBe true
    }
  }
}
