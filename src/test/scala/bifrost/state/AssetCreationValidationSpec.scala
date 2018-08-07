package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.transaction.AssetCreation
import bifrost.transaction.box._
import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.Curve25519

import scala.util.Failure

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class AssetCreationValidationSpec extends BifrostStateSpec {

  property("A block with valid AssetCreation should result in more tokens for receiver") {
    forAll(validAssetCreationGen) {
      assetCreation: AssetCreation =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L), /////Check Arbit box
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(assetCreation)
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

        //        /* Expect none of the prexisting boxes to still be around */
//        require(preExistingAssetBoxes
//          .forall(pb => newState
//            .storage
//            .get(ByteArrayWrapper(pb.id))
//            .isEmpty))

        BifrostStateSpec.genesisState = newState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

    }
  }

  property("Attempting to validate an AssetCreation with a bad signature should error") {
    forAll(validAssetCreationGen) {
      assetCreation: AssetCreation =>

        val headSig = assetCreation.signatures.head
        val wrongSig: Array[Byte] = (headSig.bytes.head + 1).toByte +: headSig.bytes.tail
        val wrongSigs: IndexedSeq[Signature25519] = Signature25519(wrongSig) +: assetCreation.signatures.tail
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
//
//  property("Attempting to validate an AssetTransfer for an amount you do not have should error") {
//    forAll(validAssetTransferGen) {
//      assetTransfer: AssetTransfer =>
//
//        val preExistingAssetBoxes: Set[BifrostBox] =
//          assetTransfer
//            .from
//            .map(f => AssetBox(f._1, f._2, 0, assetTransfer.assetCode, assetTransfer.hub))
//            .toSet
//
//        val necessaryBoxesSC = BifrostStateChanges(Set(), preExistingAssetBoxes, Instant.now.toEpochMilli)
//
//        val preparedState = BifrostStateSpec
//          .genesisState
//          .applyChanges(necessaryBoxesSC, Ints.toByteArray(10))
//          .get
//
//        val newState = preparedState.validate(assetTransfer)
//
//        BifrostStateSpec.genesisState = preparedState
//          .rollbackTo(BifrostStateSpec.genesisBlockId)
//          .get
//
//        newState shouldBe a[Failure[_]]
//        newState.failed.get.getMessage shouldBe "Not enough assets"
//    }
//  }
}
