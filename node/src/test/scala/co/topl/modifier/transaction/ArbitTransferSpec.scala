package co.topl.modifier.transaction

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{ArbitBox, Box, BoxId}
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ArbitTransferSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with ValidGenerators {

  // todo: JAA - we need to add all the the weird edge cases that were identified for the transactions
  // (this isn't being done at this moment because the structure to generate a valid transaction needs to be addressed)

  property("Randomly generated ArbitTransfer Tx should be valid") {
    forAll(validArbitTransfer(keyRing, genesisState)) { arbitTransfer: ArbitTransfer[_] =>
      arbitTransfer.syntacticValidate.isSuccess shouldBe true
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { arbitTransfer: ArbitTransfer[_] =>
      arbitTransfer.syntacticValidate.isSuccess shouldBe false
    }
  }

  property("ArbitTransfer should fail if no PolyBoxes are provided") {
    forAll(validArbitTransfer(keyRing, genesisState, fee = 0)) {
      arbitTransfer: ArbitTransfer[PublicKeyPropositionCurve25519] => {
        val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = arbitTransfer.from
          .map {
            case (address, nonce) =>
              val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
              val box = genesisState.getBox(boxId).get
              (address, nonce, box)
          }
          .collect {
            case (address, nonce, _: ArbitBox) => (address, nonce)
          }

        val noPolyRawTx = arbitTransfer.copy(from = fromWithoutPolys)
        val noPolySignedTx =
          noPolyRawTx.copy(attestation = keyRing.generateAttestation(keyRing.addresses)(noPolyRawTx.messageToSign))

        noPolySignedTx.semanticValidate(genesisState).failure.exception.getMessage shouldEqual
          "requirement failed: feeChangeOutput value does not equal input value for non-minting transaction. 1000000 != 0"
      }
    }
  }
}
