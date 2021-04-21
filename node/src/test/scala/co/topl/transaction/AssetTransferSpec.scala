package co.topl.transaction

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.modifier.transaction.AssetTransfer
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class AssetTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {
  property("Randomly generated AssetTransfer Tx should be valid") {
    forAll(validAssetTransfer(keyRing, state, minting = true)) { assetTransfer: AssetTransfer[_] =>
      assetTransfer.syntacticValidate.isSuccess shouldBe true
    }
  }

  //TODO: Test non minting AssetTransfer too

  //TODO: This can't be tested yet because we don't have the assetBox needed in state yet to make the transfer
//  property("Non minting AssetTransfer should be successful even if fee is 0") {
//    val stateWithAsset: State = genesisState(settings)
//    forAll(validAssetTransfer(keyRing, stateWithAsset, fee = 0)) {
//      assetTransfer: AssetTransfer[PublicKeyPropositionCurve25519] =>
//        assetTransfer.rawValidate.isSuccess shouldBe true
//    }
//  }

  property("Minting AssetTransfer should fail unless fee is greater than 0") {
    forAll(validAssetTransfer(keyRing, state, fee = 0, minting = true)) {
      assetTransfer: AssetTransfer[PublicKeyPropositionCurve25519] =>
        assetTransfer.syntacticValidate.failure.exception.getMessage shouldEqual
          "requirement failed: Asset minting transactions must have a non-zero positive fee"
    }
  }

  property("Attempting to validate a AssetTransfer without valid signature should error") {
    // Create invalid AssetTransfer
    // send tx to state
    forAll(assetTransferGen) { assetTransfer: AssetTransfer[_] =>
      assetTransfer.syntacticValidate.isSuccess shouldBe false
    }
  }
}
