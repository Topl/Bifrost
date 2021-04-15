package co.topl.transaction

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{ArbitBox, Box, BoxId}
import co.topl.modifier.transaction.{ArbitTransfer, Transaction}
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
    forAll(validArbitTransferGen) { arbitTransfer: ArbitTransfer[_] =>
      //TODO: Jing - change this back to using syntacticValidate once attestation in validArbitTransferGen works
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

  property("ArbitTransfer should fail if a Polybox is not provided") {
    forAll(validArbitTransfer(keyRing, state, fee = 0)) {
      arbitTransfer: ArbitTransfer[PublicKeyPropositionCurve25519] => {
        val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = arbitTransfer.from
          .map {
            case (address, nonce) =>
              val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
              val box = state.getBox(boxId).get
              (address, nonce, box)
          }
          .collect {
            case (address, nonce, _: ArbitBox) => (address, nonce)
          }

        val noPolyRawTx = arbitTransfer.copy(from = fromWithoutPolys)
        val sender = keyRing.addresses.head
        val noPolySignedTx =
          noPolyRawTx.copy(attestation = Transaction.updateAttestation(noPolyRawTx)(keyRing.generateAttestation(sender)))

        noPolySignedTx.semanticValidate(state).failure.exception.getMessage shouldEqual
          "requirement failed: Non-block reward transactions must specify at least one input box"
      }
    }
  }
}
