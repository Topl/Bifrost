package co.topl.transaction

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.modifier.transaction.AssetTransfer
import co.topl.utils.{CoreGenerators, ValidGenerators}
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
    forAll(validAssetTransferGen) { assetTransfer: AssetTransfer[_] =>
      //TODO: Jing - change this back to using syntacticValidate once attestation in validAssetTransferGen works
      assetTransfer.rawValidate.isSuccess shouldBe true
    }
  }

  property("Minting AssetTransfer should fail unless fee is greater than 0") {
    forAll(assetTransferGen) { assetTransfer: AssetTransfer[PublicKeyPropositionCurve25519] =>
      //TODO: Jing - change this back to using syntacticValidate once attestation in validAssetTransferGen works
      assetTransfer.rawValidate.isSuccess shouldBe true
      assetTransfer.copy(fee = 0).rawValidate.isSuccess shouldBe false
    }
  }

  property("Attempting to validate a AssetTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(assetTransferGen) { assetTransfer: AssetTransfer[_] =>
      assetTransfer.syntacticValidate.isSuccess shouldBe false
    }
  }
}
