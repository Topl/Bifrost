package co.topl.serialization

import co.topl.attestation._
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.legacy.attestation.keyManagement._
import co.topl.utils.codecs.binary.legacy.attestation._
import co.topl.utils.codecs.binary.legacy.modifier.block._
import co.topl.utils.codecs.binary.legacy.modifier.box._
import co.topl.utils.codecs.binary.legacy.modifier.transaction.TransactionSerializer
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.utils.codecs._
import co.topl.utils.codecs.binary.typeclasses.Persistable

import scala.util.{Failure, Success}

/**
 * Created by cykoz on 4/12/17.
 */
class SerializationTests extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with CommonGenerators {

  property("PublicKeyProposition serialization") {
    forAll(publicKeyPropositionGen) { case (_, prop: Proposition) =>
      val parsed = PropositionSerializer
        .parseBytes(PropositionSerializer.toBytes(prop))
        .get

      parsed.persistedBytes sameElements prop.persistedBytes shouldBe true
    }
  }

  property("PrivateKeyCurve25519 serialization") {
    forAll(keyCurve25519Gen) { case (key: PrivateKeyCurve25519, _) =>
      val parsed = PrivateKeyCurve25519Serializer.parseBytes(PrivateKeyCurve25519Serializer.toBytes(key)).get

      parsed.bytes sameElements key.bytes shouldBe true
    }
  }

  property("PrivateKeyEd25519 serialization") {
    forAll(keyEd25519Gen) { case (key: PrivateKeyEd25519, _) =>
      val parsed = PrivateKeyEd25519Serializer.parseBytes(PrivateKeyEd25519Serializer.toBytes(key)).get

      parsed.bytes sameElements key.bytes shouldBe true
    }
  }

  property("Signature serialization") {
    forAll(signatureGen) { sig: Proof[_] =>
      val parsed = ProofSerializer
        .parseBytes(ProofSerializer.toBytes(sig))
        .get

      Persistable[Proof[_]].persistedBytes(parsed) sameElements Persistable[Proof[_]].persistedBytes(sig) shouldBe true
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

      parsed.persistedBytes sameElements sig.persistedBytes shouldBe true
    }
  }

  property("SecurityRoot serialization") {
    forAll(securityRootGen) { root: SecurityRoot =>
      val parsed = SecurityRootSerializer.parseBytes(SecurityRootSerializer.toBytes(root)).get

      parsed shouldEqual root
    }
  }

  property("TokenValueHolder serialization") {
    forAll(Gen.oneOf(simpleValueGen, assetValueGen)) { value =>
      val parsed = TokenValueHolderSerializer.parseBytes(TokenValueHolderSerializer.toBytes(value)).get

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
    forAll(blockCurve25519Gen) { b: Block =>
      val blockHeader = b.toComponents._1
      val parsed = BlockHeaderSerializer
        .parseBytes(BlockHeaderSerializer.toBytes(blockHeader))
        .get

      parsed.persistedBytes sameElements blockHeader.persistedBytes shouldBe true
    }
  }

  property("BlockBody serialization") {
    forAll(blockCurve25519Gen) { b: Block =>
      val blockBody = b.toComponents._2
      val parsed = BlockBodySerializer
        .parseBytes(BlockBodySerializer.toBytes(blockBody))
        .get

      parsed.persistedBytes sameElements blockBody.persistedBytes shouldBe true
    }
  }

  property("FullBlock serialization") {
    forAll(blockCurve25519Gen) { bb: Block =>
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
    forAll(blockCurve25519Gen) { block =>
      val parsed: BloomFilter = BloomFilterSerializer.parseBytes(BloomFilterSerializer.toBytes(block.bloomFilter)).get

      BloomFilterSerializer.toBytes(parsed) sameElements
      BloomFilterSerializer.toBytes(block.bloomFilter) shouldBe true
    }
  }

  property("Evidence serialization") {
    forAll(evidenceGen) { evidence =>
      val parsed = EvidenceSerializer.parseBytes(EvidenceSerializer.toBytes(evidence)).get

      EvidenceSerializer.toBytes(parsed) sameElements EvidenceSerializer.toBytes(evidence) shouldBe true
    }
  }

  property("Address serialization") {
    forAll(addressGen) { address =>
      val parsed: Address = AddressSerializer.parseBytes(AddressSerializer.toBytes(address)).get

      AddressSerializer.toBytes(parsed) should contain theSameElementsInOrderAs AddressSerializer.toBytes(address)
    }
  }
}
