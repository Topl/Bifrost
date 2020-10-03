package co.topl.nodeView.state

import java.time.Instant

import co.topl.crypto.Signature25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.AssetCreation
import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.signatures.Curve25519

import scala.util.Failure


class AssetCreationValidationSpec extends StateSpec {

  property("A block with valid AssetCreation should result in more tokens for receiver") {
    forAll(validAssetCreationGen) {
      assetCreation: AssetCreation =>
        val block = Block(
          ModifierId(Array.fill(Block.signatureLength)(-1: Byte)),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L), /////Check Arbit box
          Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
          Seq(assetCreation),
          settings.forgingSettings.version
        )

        val newState = StateSpec
          .genesisState()
          .applyModifier(block)
          .get

        assetCreation.newBoxes.forall(b => newState.getBox(b.id) match {
          case Some(box) => box.bytes sameElements b.bytes
          case None => false
        })

    }
  }

  property("Attempting to validate an AssetCreation with a bad signature should error") {
    forAll(validAssetCreationGen) {
      assetCreation: AssetCreation =>

        val headSig = assetCreation.signatures.head
        val wrongSig: Array[Byte] = (headSig._2.bytes.head + 1).toByte +: headSig._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = assetCreation.signatures + (headSig._1 -> Signature25519(wrongSig))
        val invalidAC = assetCreation.copy(signatures = wrongSigs)

//        val necessaryBoxesSC = StateChanges(Set(), Set())
//
//        val preparedState = StateSpec
//          .genesisState()
//          .applyChanges(ModifierId(Ints.toByteArray(9)), necessaryBoxesSC)
//          .get
//
//        val newState = preparedState.validate(invalidAC)
//
//        val firstCCAddBlock = Block(
//          ModifierId(Array.fill(Block.signatureLength)(1: Byte)),
//          Instant.now.toEpochMilli,
//          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), scala.util.Random.nextLong(), 0L),
//          Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
//          Seq(assetCreation),
//          settings.forgingSettings.version
//          )
//
//        val necessaryState = StateSpec
//          .genesisState()
//          .applyModifier(firstCCAddBlock)
//          .get

        val newState = StateSpec
          .genesisState()
          .validate(invalidAC)

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "requirement failed: Invalid signatures"
    }
  }
}
