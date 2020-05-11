package bifrost.state

import java.time.Instant

import bifrost.block.Block
import bifrost.crypto.Signature25519
import bifrost.transaction.bifrostTransaction.AssetCreation
import modifier.box._
import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import modifier.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519

import scala.util.Failure


class AssetCreationValidationSpec extends BifrostStateSpec {

  property("A block with valid AssetCreation should result in more tokens for receiver") {
    forAll(validAssetCreationGen) {
      assetCreation: AssetCreation =>
        val block = Block(
          Array.fill(Block.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L), /////Check Arbit box
          Signature25519(Array.fill(Block.SignatureLength)(0: Byte)),
          Seq(assetCreation),
          10L,
          settings.version
        )

        val assetBoxes: Traversable[AssetBox] = assetCreation.newBoxes.map {
          case a: AssetBox => a
          case _ => throw new Exception("Was expecting AssetBoxes but found something else")
        }

        val necessaryBoxesSC = BifrostStateChanges(Set(), Set(), Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(7))
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(8))
          .get

        assetCreation.newBoxes.forall(b => newState.storage.get(ByteArrayWrapper(b.id)) match {
          case Some(wrapper) => wrapper.data sameElements b.bytes
          case None => false
        })

        BifrostStateSpec.genesisState = newState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

    }
  }

  property("Attempting to validate an AssetCreation with a bad signature should error") {
    forAll(validAssetCreationGen) {
      assetCreation: AssetCreation =>

        val headSig = assetCreation.signatures.head
        val wrongSig: Array[Byte] = (headSig._2.bytes.head + 1).toByte +: headSig._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = assetCreation.signatures + (headSig._1 -> Signature25519(wrongSig))
        val invalidAC = assetCreation.copy(signatures = wrongSigs)

        val necessaryBoxesSC = BifrostStateChanges(Set(), Set(), Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(9))
          .get

        val newState = preparedState.validate(invalidAC)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "requirement failed: Invalid signatures"
    }
  }
}
