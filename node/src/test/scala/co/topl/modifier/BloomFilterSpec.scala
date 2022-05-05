package co.topl.modifier

import co.topl.attestation.Address
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.codecs._
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.block.{BloomFilter, TransactionsCarryingPersistentNodeViewModifier}
import co.topl.nodeView.ValidTransactionGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BloomFilterSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with ValidTransactionGenerators
    with Matchers {

  property("Bloomfilter should be able to tell if it definitely contains an address(false negatives)") {
    forAll(bifrostTransactionSeqGen) { txs =>
      val bloomfilter: BloomFilter = TransactionsCarryingPersistentNodeViewModifier.createBloom(txs)

      txs.foreach { tx =>
        tx.bloomTopics.foreach { bt =>
          bloomfilter.contains(bt) shouldBe true
        }
      }
    }
  }

  property("Bloomfilter should be able to tell if an address is likely not in the block(false positives)") {
    forAll(bifrostTransactionSeqGen) { txs =>
      val bloomfilter: BloomFilter = TransactionsCarryingPersistentNodeViewModifier.createBloom(txs.dropRight(1))
      val addressInBloom: Int = txs.dropRight(1).foldLeft(0)(_ + _.bloomTopics.size)
      val numAddressLastTx: Int = txs.last.bloomTopics.size

      val falsePositives = txs.last.bloomTopics.count(bloomfilter.contains)

      /**
       * Sometimes there's very few addresses in the last transaction, we only test here to make sure we don't get too
       * many false positives. There's a very slight chance that this will break (if it does this is probably an issue)
       */
      (falsePositives <= addressInBloom / 3) shouldBe true
    }
  }

  /**
   * See: https://hur.st/bloomfilter/?n=500&p=&m=2048&k=4
   * Assuming that there will be 500 address in the bloom filter:
   *   the probability of false positives will be 0.151 (1 in 7)
   */
  property("The probability of false positives in bloomfilter with 500 addresses should be lower than 0.15") {

    /** The generated addresses are made deterministic, so that the variation won't break the test */
    val rand = new scala.util.Random(1)

    /** 500 addresses in bloomfilter and 100 addresses for the test */
    val numAddr = 600
    val numBloom = 500

    val randAddr: Seq[Address] =
      (0 until numAddr)
        .map(_ => Array.fill(32)((rand.nextInt(256) - 128).toByte))
        .map(s => PrivateKeyCurve25519.secretGenerator.generateSecret(s)._2)
        .map(k => k.address)

    val bloomTopics: Set[BloomTopic] = randAddr.take(numBloom).map(addr => BloomTopic(addr.persistedBytes)).toSet
    val bloomfilter: BloomFilter = BloomFilter(bloomTopics)
    val testTopics: Seq[BloomTopic] = randAddr.drop(numBloom).map(addr => BloomTopic(addr.persistedBytes))

    val falsePositives = testTopics.count(bloomfilter.contains)

    falsePositives shouldBe 15 +- 3
  }
}
