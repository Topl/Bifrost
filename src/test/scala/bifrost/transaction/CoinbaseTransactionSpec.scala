package bifrost.transaction

import bifrost.blocks.BifrostBlock
import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.{BifrostState, BifrostStateSpec}
import bifrost.transaction.bifrostTransaction.CoinbaseTransaction
import bifrost.transaction.box.ArbitBox
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.transaction.state.PrivateKey25519

class CoinbaseTransactionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated Coinbase Tx should be valid") {
    forAll(validCoinbaseTransactionGen) {
      cb: CoinbaseTransaction => BifrostState.semanticValidity(cb).isSuccess shouldBe true
    }

    // test inflation val stuff works
    val preparedState = BifrostStateSpec.genesisState
    val testPrivKeyBytes = BifrostStateSpec.gw.secrets.head.privKeyBytes
    val testPubKeyBytes = BifrostStateSpec.gw.secrets.head.publicKeyBytes
    val test25519 = PrivateKey25519(testPrivKeyBytes, testPubKeyBytes)
    val forgerBox = ArbitBox(test25519.publicImage, 123456789, 100)
    val parentBlock = BifrostBlock.create(BifrostStateSpec.genesisBlockId, 10000L, Seq.empty[BifrostTransaction], forgerBox, test25519, 10)
    preparedState.changes(parentBlock)
    BifrostStateSpec.history.append(parentBlock)
    val CB = CoinbaseTransaction.createAndApply(BifrostStateSpec.gw, IndexedSeq((test25519.publicImage, 666L)), parentBlock.id).get
    val testBlock = BifrostBlock.create(parentBlock.id, 10001L, Seq(CB), forgerBox, test25519, 666)
    preparedState.changes(testBlock)
    BifrostStateSpec.history.append(testBlock)
    preparedState.validate(CB).isSuccess shouldBe true

  }
}
