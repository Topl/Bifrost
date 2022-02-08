package co.topl.serialization

import cats.implicits._
import co.topl.attestation._
import co.topl.attestation.keyManagement.{
  KeyfileCurve25519,
  KeyfileCurve25519Companion,
  KeyfileEd25519,
  KeyfileEd25519Companion
}
import co.topl.codecs._
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction
import co.topl.utils.implicits._
import co.topl.utils.{CommonGenerators, EqMatcher}
import io.circe.syntax.EncoderOps
import io.circe.{DecodingFailure, Encoder}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ListMap

class JsonTests
    extends AnyPropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with CommonGenerators
    with EqMatcher {

  property("PublicKeyProposition json") {
    forAll(propositionCurve25519Gen)(prop =>
      prop.asJson.as[PublicKeyPropositionCurve25519] should eqvShow(prop.asRight[DecodingFailure])
    )
    forAll(propositionEd25519Gen)(prop =>
      prop.asJson.as[PublicKeyPropositionEd25519] should eqvShow(prop.asRight[DecodingFailure])
    )
  }

  property("ThresholdProposition json") {
    forAll(thresholdPropositionCurve25519Gen) { case (_, prop) =>
      prop.asJson.as[ThresholdPropositionCurve25519] should eqvShow(prop.asRight[DecodingFailure])
    }
  }

  property("Signature json") {
    forAll(signatureCurve25519Gen)(sig =>
      sig.asJson.as[SignatureCurve25519] should eqvShow(sig.asRight[DecodingFailure])
    )
    forAll(signatureEd25519Gen)(sig => sig.asJson.as[SignatureEd25519] should eqvShow(sig.asRight[DecodingFailure]))
  }

  property("Attestation Ed25519 json") {
    forAll(attestationEd25519Gen) { attestation =>
      // must encode as a generic proposition and proof
      val json = Encoder[ListMap[Proposition, Proof[Proposition]]].apply(
        attestation.asInstanceOf[ListMap[Proposition, Proof[Proposition]]]
      )

      val decodedAttestation = json.as[ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]]

      decodedAttestation shouldEqual Right(attestation)
    }
  }

  property("Attestation Curve25519 json") {
    forAll(attestationCurve25519Gen) { attestation =>
      // must encode as a generic proposition and proof
      val json = Encoder[ListMap[Proposition, Proof[Proposition]]].apply(
        attestation.asInstanceOf[ListMap[Proposition, Proof[Proposition]]]
      )

      json.as[ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]] shouldEqual Right(
        attestation
      )
    }
  }

  property("Attestation ThresholdCurve25519 json") {
    forAll(attestationThresholdCurve25519Gen) { attestation =>
      // must encode as a generic proposition and proof
      val json = Encoder[ListMap[Proposition, Proof[Proposition]]].apply(
        attestation.asInstanceOf[ListMap[Proposition, Proof[Proposition]]]
      )

      json
        .as[ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]] shouldEqual Right(
        attestation
      )
    }
  }

  property("Keyfile json") {
    forAll(keyCurve25519Gen) { key =>
      val keyfile = KeyfileCurve25519Companion.encryptSecret(key._1, "test")
      keyfile.asJson.as[KeyfileCurve25519] match {
        case Right(kf) =>
          kf.address should eqvShow(keyfile.address)
          kf.cipherText should eqvShow(keyfile.cipherText)
          kf.mac should eqvShow(keyfile.mac)
          kf.salt should eqvShow(keyfile.salt)
          kf.iv should eqvShow(keyfile.iv)
        case Left(e) => e
      }
    }

    forAll(keyEd25519Gen) { key =>
      val keyfile = KeyfileEd25519Companion.encryptSecret(key._1, "test")
      keyfile.asJson.as[KeyfileEd25519] match {
        case Right(kf) =>
          kf.address should eqvShow(keyfile.address)
          kf.cipherText should eqvShow(keyfile.cipherText)
          kf.mac should eqvShow(keyfile.mac)
          kf.salt should eqvShow(keyfile.salt)
          kf.iv should eqvShow(keyfile.iv)
        case Left(e) => e
      }
    }
  }

  property("Address json") {
    forAll(addressGen)(address => address.asJson.as[Address] should eqvShow(address.asRight[DecodingFailure]))
  }

  property("Evidence json") {
    forAll(evidenceGen)(evidence => evidence.asJson.as[Evidence] should eqvShow(evidence.asRight[DecodingFailure]))
  }

  property("ModifierId json") {
    forAll(modifierIdGen)(id => id.asJson.as[ModifierId] should eqvShow(id.asRight[DecodingFailure]))
  }

  property("AssetCode json") {
    forAll(assetCodeGen)(code => code.asJson.as[AssetCode] should eqvShow(code.asRight[DecodingFailure]))
  }

  property("SecurityRoot json") {
    forAll(securityRootGen)(root => root.asJson.as[SecurityRoot] should eqvShow(root.asRight[DecodingFailure]))
  }

  property("TokenValueHolder json") {
    forAll(Gen.oneOf(simpleValueGen, assetValueGen)) { value: TokenValueHolder =>
      value.asJson.as[TokenValueHolder] should eqvShow(value.asRight[DecodingFailure])
    }
  }

  property("PolyBox json") {
    forAll(polyBoxGen)(box => box.asJson.as[PolyBox] should eqvShow(box.asRight[DecodingFailure]))
  }

  property("ArbitBox json") {
    forAll(arbitBoxGen)(box => box.asJson.as[ArbitBox] should eqvShow(box.asRight[DecodingFailure]))
  }

  property("AssetBox json") {
    forAll(assetBoxGen)(box => box.asJson.as[AssetBox] should eqvShow(box.asRight[DecodingFailure]))
  }

  property("ProgramId json") {
    forAll(programIdGen)(id => id.asJson.as[ProgramId] should eqvShow(id.asRight[DecodingFailure]))
  }

  property("StateBox json") {
    forAll(stateBoxGen)(box => box.asJson.as[StateBox] should eqvShow(box.asRight[DecodingFailure]))
  }

  property("CodeBox json") {
    forAll(codeBoxGen)(box => box.asJson.as[CodeBox] should eqvShow(box.asRight[DecodingFailure]))
  }

  property("ExecutionBox json") {
    forAll(executionBoxGen)(box => box.asJson.as[ExecutionBox] should eqvShow(box.asRight[DecodingFailure]))
  }

  property("Transaction json") {
    forAll(transferGen) { tx =>
      tx.asJson.as[Transaction.TX] should eqvShow[Either[DecodingFailure, Transaction.TX]](tx.asRight[DecodingFailure])
    }
  }

  property("FullBlock json") {
    forAll(blockCurve25519Gen)(block => block.asJson.as[Block] should eqvShow(block.asRight[DecodingFailure]))
  }

  property("BlockHeader json") {
    forAll(blockCurve25519Gen) { block =>
      val header = block.toComponents._1
      header.asJson.as[BlockHeader] should eqvShow(header.asRight[DecodingFailure])
    }
  }

  property("BlockBody json") {
    forAll(blockCurve25519Gen) { block =>
      val body = block.toComponents._2
      body.asJson.as[BlockBody] should eqvShow(body.asRight[DecodingFailure])
    }
  }

  property("BloomFilter json") {
    forAll(blockCurve25519Gen) { block =>
      val bloomFilter = block.bloomFilter

      bloomFilter.asJson.as[BloomFilter] should eqvShow(bloomFilter.asRight[DecodingFailure])
    }
  }
}
