package co.topl.modifier.transaction

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{ArbitBox, Box, BoxId}
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ArbitTransferSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with ValidGenerators
    with EitherValues {

  // todo: JAA - we need to add all the the weird edge cases that were identified for the transactions
  // (this isn't being done at this moment because the structure to generate a valid transaction needs to be addressed)

}
