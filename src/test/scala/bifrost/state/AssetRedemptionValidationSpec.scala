package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.transaction.AssetRedemption
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
class AssetRedemptionValidationSpec extends BifrostStateSpec {

  property("A block with valid AssetRedemption should result in the appropriate amount of tokens") {
    forAll(validAssetRedemptionGen) {
      ar: AssetRedemption =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(ar)
        )

        val preExistingAssetBoxes: Set[BifrostBox] = ar
          .availableToRedeem
          .flatMap {
            case (assetCode, toRedeem) =>
              toRedeem
                .map(r => AssetBox(r._1, r._2, ar.remainderAllocations(assetCode).map(_._2).sum, assetCode, ar.hub))
          }
          .toSet


        val assetBoxes: Traversable[AssetBox] = ar.newBoxes.map {
          case a: AssetBox => a
          case _ => throw new Exception("Was expecting AssetBoxes but found something else")
        }

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingAssetBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(2))
          .get

        ar.newBoxes
          .forall(b => newState.storage.get(ByteArrayWrapper(b.id)) match {
            case Some(wrapper) => wrapper.data sameElements b.bytes
            case None => false
          })

        /* Expect none of the prexisting boxes to still be around */
        require(preExistingAssetBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)).isEmpty))

        BifrostStateSpec.genesisState = newState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

    }
  }

  property("Attempting to validate an AssetRedemption with a bad signature should error") {
    forAll(validAssetRedemptionGen) {
      assetRedemption: AssetRedemption =>

        val headSigs = assetRedemption.signatures.head
        val wrongSig: Array[Byte] = (headSigs._2.head.bytes.head + 1).toByte +: headSigs._2.head.bytes.tail
        val wrongSigs: Map[String, IndexedSeq[Signature25519]] =
          assetRedemption.signatures +
            (headSigs._1 -> (assetRedemption.signatures(headSigs._1).tail :+ Signature25519(wrongSig)))

        val invalidAR = assetRedemption.copy(signatures = wrongSigs)

        val preExistingAssetBoxes: Set[BifrostBox] = assetRedemption
          .availableToRedeem
          .flatMap {
            case (assetCode, toRedeem) =>
              toRedeem.map(r => AssetBox(r._1,
                                         r._2,
                                         assetRedemption.remainderAllocations(assetCode).map(_._2).sum,
                                         assetCode,
                                         assetRedemption.hub))
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingAssetBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(invalidAR)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Incorrect unlocker"
    }
  }

  property("Attempting to validate an AssetRedemption with insufficient assets should error") {
    forAll(validAssetRedemptionGen) {
      ar: AssetRedemption =>

        val preExistingAssetBoxes: Set[BifrostBox] = ar.availableToRedeem.flatMap {
          case (assetCode, toRedeem) => toRedeem.map(r => AssetBox(r._1, r._2, 0, assetCode, ar.hub))
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(Set(), preExistingAssetBoxes, Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(1))
          .get

        val newState = preparedState.validate(ar)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Not enough assets"
    }
  }
}
