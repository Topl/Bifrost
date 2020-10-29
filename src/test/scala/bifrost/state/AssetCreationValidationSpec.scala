package bifrost.state

import java.time.Instant

import bifrost.crypto.Signature25519
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.AssetCreation
import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.signatures.Curve25519

import scala.util.Failure

class AssetCreationValidationSpec extends StateSpec {

  property("A block with valid AssetCreation should result in more tokens for receiver") {
    forAll(validAssetCreationGen) { assetCreation: AssetCreation =>
      val block = Block(
        ModifierId(Array.fill(Block.signatureLength)(-1: Byte)),
        Instant.now.toEpochMilli,
        ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L), /////Check Arbit box
        Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
        Seq(assetCreation),
        settings.forgingSettings.version
      )

      val necessaryBoxesSC = StateChanges(Set(), Set(), Instant.now.toEpochMilli)

      val preparedState = StateSpec.genesisState
        .applyChanges(necessaryBoxesSC, ModifierId(Ints.toByteArray(7)))
        .get

      val newState = preparedState
        .applyChanges(StateChanges(block).get, ModifierId(Ints.toByteArray(8)))
        .get

      assetCreation.newBoxes.forall(b =>
        newState.storage.get(ByteArrayWrapper(b.id)) match {
          case Some(wrapper) => wrapper.data sameElements b.bytes
          case None          => false
        }
      )

      StateSpec.genesisState = newState
        .rollbackTo(StateSpec.genesisBlockId)
        .get

    }
  }

  property("Attempting to validate an AssetCreation with a bad signature should error") {
    forAll(validAssetCreationGen) { assetCreation: AssetCreation =>
      val headSig = assetCreation.signatures.head
      val wrongSig: Array[Byte] = (headSig._2.bytes.head + 1).toByte +: headSig._2.bytes.tail
      val wrongSigs: Map[PublicKey25519Proposition, Signature25519] =
        assetCreation.signatures + (headSig._1 -> Signature25519(wrongSig))
      val invalidAC = assetCreation.copy(signatures = wrongSigs)

      val necessaryBoxesSC = StateChanges(Set(), Set(), Instant.now.toEpochMilli)

      val preparedState = StateSpec.genesisState
        .applyChanges(necessaryBoxesSC, ModifierId(Ints.toByteArray(9)))
        .get

      val newState = preparedState.validate(invalidAC)

      StateSpec.genesisState = preparedState
        .rollbackTo(StateSpec.genesisBlockId)
        .get

      newState shouldBe a[Failure[_]]
      newState.failed.get.getMessage shouldBe "requirement failed: Invalid signatures"
    }
  }
}
