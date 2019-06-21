package bifrost.history

/**
  * Created by cykoz on 7/11/2017.
  */

import bifrost.blocks.{BifrostBlock, Bloom}
import bifrost.state.BifrostStateSpec
import bifrost.transaction.bifrostTransaction.{AssetCreation}
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import bifrost.transaction.box.proposition.PublicKey25519Proposition

import scala.collection.BitSet
import scala.util.Try

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

  /*property("Checking bloom filter for specific transactions") {
    val tx: AssetCreation = AssetCreation.createAndApply(BifrostStateSpec.gw,
    IndexedSeq((PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get), 10L)),
    0L,
    PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get),
    "test_2",
    ""
    ).get
    val block = BifrostBlock(history.bestBlockId,
    System.currentTimeMillis(),
    arbitBoxGen.sample.get,
    signatureGen.sample.get,
    Seq(tx),
    10L

    forAll(validBifrostTransactionSeqGen) { txs =>
      val block = BifrostBlock(history.bestBlockId,
                               System.currentTimeMillis(),
                               arbitBoxGen.sample.get,
                               signatureGen.sample.get,
                               txs,
                               10L,
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