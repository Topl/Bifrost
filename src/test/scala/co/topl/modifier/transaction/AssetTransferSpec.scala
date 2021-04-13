package co.topl.modifier.transaction

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.box.Box.identifier
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.Try

class AssetTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  property("Randomly generated AssetTransfer Tx should be valid") {
    forAll(validAssetTransfer(keyRing, genesisState, minting = true)) { assetTransfer: AssetTransfer[_] =>
      assetTransfer.syntacticValidate.isSuccess shouldBe true
    }
  }

  property("Minting AssetTransfer should fail unless fee is greater than 0") {
    forAll(validAssetTransfer(keyRing, genesisState, fee = 0, minting = true)) {
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
