package co.topl.modifier.transaction

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{AssetBox, Box, BoxId}
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class AssetTransferSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with ValidGenerators {

  // todo: JAA - we need to add all the the weird edge cases that were identified for the transactions
  // (this isn't being done at this moment because the structure to generate a valid transaction needs to be addressed)

  property("Randomly generated AssetTransfer Tx should be valid") {
    forAll(validAssetTransfer(keyRing, genesisState, minting = true)) { assetTransfer: AssetTransfer[_] =>
      assetTransfer.syntacticValidate.isValid shouldBe true
    }
  }

  property("AssetTransfer with minting = true should fail if no PolyBoxes are provided") {
    forAll(validAssetTransfer(keyRing, genesisState, fee = 0, minting = true)) {
      assetTransfer: AssetTransfer[PublicKeyPropositionCurve25519] =>
        val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = assetTransfer.from
          .map { case (address, nonce) =>
            val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
            val box = genesisState.getBox(boxId).get
            (address, nonce, box)
          }
          .collect { case (address, nonce, _: AssetBox) =>
            (address, nonce)
          }

        val noPolyRawTx = assetTransfer.copy(from = fromWithoutPolys)
        val sender = keyRing.addresses.head
        val noPolySignedTx =
          noPolyRawTx.copy(attestation =
            Transaction.updateAttestation(noPolyRawTx)(keyRing.generateAttestation(sender))
          )

        noPolySignedTx.semanticValidate(genesisState).failure.exception.getMessage shouldEqual "MintingZeroFeeFailure"
    }
  }

  property("Attempting to validate a AssetTransfer without valid signature should error") {
    // Create invalid AssetTransfer
    // send tx to state
    forAll(assetTransferGen) { assetTransfer: AssetTransfer[_] =>
      assetTransfer.syntacticValidate.isValid shouldBe false
    }
  }
}
