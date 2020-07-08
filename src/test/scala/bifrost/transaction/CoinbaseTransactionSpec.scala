package bifrost.transaction

import bifrost.modifier.transaction.bifrostTransaction.CoinbaseTransaction
import bifrost.state.State
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class CoinbaseTransactionSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated Coinbase Tx should be valid") {
    forAll(validCoinbaseTransactionGen) {
      cb: CoinbaseTransaction => State.semanticValidity(cb).isSuccess shouldBe true
    }

    // test inflation val stuff works
    /*val preparedState = BifrostStateSpec.genesisState
    val testPrivKeyBytes = BifrostStateSpec.gw.secrets.head.privKeyBytes
    val testPubKeyBytes = BifrostStateSpec.gw.secrets.head.publicKeyBytes
    val test25519 = PrivateKey25519(testPrivKeyBytes, testPubKeyBytes)
    val forgerBox = ArbitBox(test25519.publicImage, 123456789, 100)
    val parentBlock = Block.create(BifrostStateSpec.genesisBlockId, 10000L, Seq.empty[Transaction], forgerBox, test25519, 10, settings.version)
    preparedState.changes(parentBlock)
    BifrostStateSpec.history.append(parentBlock)
    val CB = CoinbaseTransaction.createAndApply(BifrostStateSpec.gw, IndexedSeq((test25519.publicImage, 666L)), parentBlock.id).get
    val testBlock = Block.create(parentBlock.id, 10001L, Seq(CB), forgerBox, test25519, 666, settings.version)
    preparedState.changes(testBlock)
    BifrostStateSpec.history.append(testBlock)
    preparedState.validate(CB).isSuccess shouldBe true
     */

  }
}
