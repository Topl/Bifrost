package co.topl.serialization

import co.topl.attestation._
import co.topl.attestation.serialization.{PublicKeyPropositionCurve25519Serializer, SignatureCurve25519Serializer, ThresholdPropositionCurve25519Serializer, ThresholdSignatureCurve25519Serializer}
import co.topl.keyManagement.PrivateKeyCurve25519
import co.topl.modifier.block.serialization.{BlockBodySerializer, BlockHeaderSerializer, BlockSerializer}
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.box.serialization.BoxSerializer
import co.topl.modifier.transaction._
import co.topl.modifier.transaction.serialization._
import co.topl.settings.VersionSerializer
import co.topl.utils.{CoreGenerators, ValidGenerators}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.{Failure, Success}

/**
 * Created by cykoz on 4/12/17.
 */
class SerializationTests
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators
    with ValidGenerators {

  property("PublicKeyPropositionCurve25519 serialization") {
    forAll(publicKeyPropositionCurve25519Gen) { case (_, prop: PublicKeyPropositionCurve25519) =>
      val parsed = PublicKeyPropositionCurve25519Serializer
        .parseBytes(PublicKeyPropositionCurve25519Serializer.toBytes(prop))
        .get

      parsed.bytes sameElements prop.bytes shouldBe true
    }
  }

  property("PrivateKeyCurve25519 serialization") {
    forAll(key25519Gen) { case (key: PrivateKeyCurve25519, _) =>
      val parsed = PrivateKeyCurve25519.parseBytes(PrivateKeyCurve25519.toBytes(key)).get

      parsed.bytes sameElements key.bytes shouldBe true
    }
  }

  property("SignatureCurve25519 serialization") {
    forAll(signatureGen) { sig: SignatureCurve25519 =>
      val parsed = SignatureCurve25519Serializer
        .parseBytes(SignatureCurve25519Serializer.toBytes(sig))
        .get

      parsed.bytes sameElements sig.bytes shouldBe true
    }
  }

  property("ThresholdPropositionCurve25519 serialization") {
    forAll(thresholdPropositionCurve25519Gen) { case (_, prop: ThresholdPropositionCurve25519) =>
      val parsed = ThresholdPropositionCurve25519Serializer
        .parseBytes(ThresholdPropositionCurve25519Serializer.toBytes(prop))
        .get

      parsed.threshold shouldBe prop.threshold
      parsed.pubKeyProps should contain theSameElementsAs prop.pubKeyProps
    }
  }

  property("ThresholdSignatureCurve25519 serialization") {
    forAll(thresholdSignatureCurve25519Gen) { sig: ThresholdSignatureCurve25519 =>
      val parsed = ThresholdSignatureCurve25519Serializer
        .parseBytes(ThresholdSignatureCurve25519Serializer.toBytes(sig))
        .get

      parsed.bytes sameElements sig.bytes shouldBe true
    }
  }

  property("SecurityRoot serialization") {
    forAll(securityRootGen) { root: SecurityRoot =>
      val parsed = SecurityRoot.parseBytes(SecurityRoot.toBytes(root)).get

      parsed shouldEqual root
    }
  }

  property("TokenValueHolder serialization") {
    forAll(Gen.oneOf(simpleValueGen, assetValueGen)) { value =>
      val parsed = TokenValueHolder.parseBytes(TokenValueHolder.toBytes(value)).get

      parsed shouldEqual value
    }
  }

  property("PolyBox serialization") {
    forAll(polyBoxGen) { b: PolyBox =>
      val parsed = BoxSerializer
        .parseBytes(BoxSerializer.toBytes(b))
        .get

      val serialized = BoxSerializer.toBytes(parsed)
      serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ArbitBox serialization") {
    forAll(arbitBoxGen) { b: ArbitBox =>
      val parsed = BoxSerializer
        .parseBytes(BoxSerializer.toBytes(b))
        .get

      val serialized = BoxSerializer.toBytes(parsed)
      serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("AssetBox serialization") {
    forAll(assetBoxGen) { b: AssetBox =>
      val parsed = BoxSerializer
        .parseBytes(BoxSerializer.toBytes(b))
        .get

      val serialized = BoxSerializer.toBytes(parsed)
      serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("StateBox serialization") {
    forAll(stateBoxGen) { b: StateBox =>
      val parsed = BoxSerializer
        .parseBytes(BoxSerializer.toBytes(b))
        .get

      val serialized = BoxSerializer.toBytes(parsed)
      serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("CodeBox serialization") {
    forAll(codeBoxGen) { b: CodeBox =>
      val parsed = BoxSerializer
        .parseBytes(BoxSerializer.toBytes(b))
        .get

      val serialized = BoxSerializer.toBytes(parsed)
      serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("ExecutionBox serialization") {
    forAll(executionBoxGen) { b: ExecutionBox =>
      val parsed = BoxSerializer
        .parseBytes(BoxSerializer.toBytes(b))
        .get

      val serialized = BoxSerializer.toBytes(parsed)
      serialized sameElements BoxSerializer.toBytes(b) shouldBe true
    }
  }

  property("PolyTransfer serialization") {
    forAll(polyTransferGen) { tx: PolyTransfer[_ <: Proposition] =>
      val serializer = TransactionSerializer
      val parsed = serializer
        .parseBytes(serializer.toBytes(tx))
        .get

      serializer.toBytes(parsed) sameElements
      serializer.toBytes(tx) shouldBe true
    }
  }

  property("ArbitTransfer serialization") {
    forAll(arbitTransferGen) { tx: ArbitTransfer[_ <: Proposition] =>
      val serializer = TransactionSerializer
      val parsed = serializer
        .parseBytes(serializer.toBytes(tx))
        .get

      serializer.toBytes(parsed) sameElements
      serializer.toBytes(tx) shouldBe true
    }
  }

  property("AssetTransfer serialization") {
    forAll(assetTransferGen) { tx: AssetTransfer[_ <: Proposition] =>
      val serializer = TransactionSerializer
      val parsed = serializer
        .parseBytes(serializer.toBytes(tx))
        .get

      serializer.toBytes(parsed) sameElements
      serializer.toBytes(tx) shouldBe true
    }
  }

  property("BlockHeader serialization") {
    forAll(blockGen) { b: Block =>
      val blockHeader = b.toComponents._1
      val parsed = BlockHeaderSerializer
        .parseBytes(BlockHeaderSerializer.toBytes(blockHeader))
        .get

      parsed.bytes sameElements blockHeader.bytes shouldBe true
    }
  }

  property("BlockBody serialization") {
    forAll(blockGen) { b: Block =>
      val blockBody = b.toComponents._2
      val parsed = BlockBodySerializer
        .parseBytes(BlockBodySerializer.toBytes(blockBody))
        .get

      parsed.bytes sameElements blockBody.bytes shouldBe true
    }
  }

  property("FullBlock serialization") {
    forAll(blockGen) { bb: Block =>
      val parsed = BlockSerializer.parseBytes(BlockSerializer.toBytes(bb))

      parsed match {
        case Success(p) =>
          BlockSerializer.toBytes(p) sameElements
            BlockSerializer.toBytes(bb) shouldBe true
        case Failure(e) => throw e
      }
    }
  }

  property("BloomFilter serialization") {
    forAll(blockGen) { block =>
      val parsed: BloomFilter = BloomFilter.parseBytes(BloomFilter.toBytes(block.bloomFilter)).get

      BloomFilter.toBytes(parsed) sameElements
      BloomFilter.toBytes(block.bloomFilter) shouldBe true
    }
  }

  property("Evidence serialization") {
    forAll(evidenceGen) { evidence =>
      val parsed = Evidence.parseBytes(Evidence.toBytes(evidence)).get

      Evidence.toBytes(parsed) sameElements Evidence.toBytes(evidence) shouldBe true
    }
  }

  property("Address serialization") {
    forAll(addressGen) { address =>
      val parsed: Address = Address.parseBytes(Address.toBytes(address)).get

      Address.toBytes(parsed) sameElements Address.toBytes(address) shouldBe true
    }
  }

  property("Version serialization") {
    forAll(versionGen) { version =>
      val parsed = VersionSerializer.parseBytes(VersionSerializer.toBytes(version)).get

      parsed.bytes sameElements version.bytes
    }
  }
}
