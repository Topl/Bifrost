package co.topl.nodeView.history

/**
  * Created by cykoz on 7/11/2017.
  */

import co.topl.modifier.block.Bloom
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.BitSet
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.BitSet

class BloomFilterSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  var history: History = generateHistory(0: Byte)

  property("Verify Bloom Calculation is correct") {
    val set = Bloom.calcBloom(Array.fill(32)(1), IndexedSeq(Array.fill(32)(1)))
    set shouldEqual BitSet(10, 138, 201)
  }

  /*property("Checking bloom filter for specific transactions") {
    val tx: AssetCreation = AssetCreation.createAndApply(BifrostStateSpec.gw,
    IndexedSeq((PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get), 10L)),
    0L,
    PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get),
    "test_2",
    ""
    ).get
    val block = Block(history.bestBlockId,
    System.currentTimeMillis(),
    arbitBoxGen.sample.get,
    signatureGen.sample.get,
    Seq(tx),

    forAll(validBifrostTransactionSeqGen) { txs =>
      val block = Block(history.bestBlockId,
                               System.currentTimeMillis(),
                               arbitBoxGen.sample.get,
                               signatureGen.sample.get,
                               txs,
                               settings.version
      )

    println(tx.bloomTopics.get(1).mkString(""))
    println(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get.mkString(""))

    println()
    println(tx.bloomTopics.get(0).mkString(""))
    println("AssetCreation".getBytes.mkString(""))
    println()
    history = history.append(block).get._1
//    val txs = history.bloomFilter(tx.bloomTopics.get)

    val txs = history.bloomFilter(IndexedSeq("AssetCreation".getBytes))
    txs.foreach(tx => println(tx.json))
    txs.head.bytes sameElements tx.bytes shouldBe true


  }*/
}