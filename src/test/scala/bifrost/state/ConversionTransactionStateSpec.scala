package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
//import bifrost.transaction.ConversionTransaction
import bifrost.transaction.box.{ArbitBox, AssetBox, BifrostBox, PolyBox}
import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.Curve25519

import scala.util.Failure

/**
  * Created by Nicholas on 7/6/2017.
  */
class ConversionTransactionStateSpec extends BifrostStateSpec {

  /*property("A block with valid ConversionTransaction should result in the correct number of polys being created") {
    forAll(validConversionTxGen) {
      conversionTransaction: ConversionTransaction =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(conversionTransaction))

        val preExistingAssetBoxes: Set[BifrostBox] = conversionTransaction
          .totalAssetBoxes
          .flatMap {
            case (assetHub, propNonce) =>
              propNonce
                .map(r => AssetBox(
                  r._1,
                  r._2,
                  conversionTransaction
                    .assetsToReturn(assetHub)
                    .map(_._2)
                    .sum +
                    conversionTransaction
                      .assetTokensToRedeem(assetHub)
                      .map(_._2).sum,
                  assetHub._1,
                  assetHub._2,
                  conversionTransaction.data))
          }
          .toSet

        //println(preExistingAssetBoxes)

        val assetBoxes: Traversable[BifrostBox] = conversionTransaction.newBoxes.map {
          case a: AssetBox => a
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting either AssetBox or PolyBox, but found something else")
        }

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingAssetBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(11))
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(12))
          .get

        conversionTransaction
          .newBoxes
          .forall(b => newState.storage.get(ByteArrayWrapper(b.id)) match {
            case Some(wrapper) => wrapper.data sameElements b.bytes
            case None => false
          })

        require(preExistingAssetBoxes
                  .forall(pb => newState
                    .storage
                    .get(ByteArrayWrapper(pb.id))
                    .isEmpty))

        BifrostStateSpec.genesisState = newState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get
    }
  }

  property("Attempting to validate a ConversionTransaction with a bad signature should error") {
    forAll(validConversionTxGen) {
      conversionTransaction: ConversionTransaction =>

        val headSigs = conversionTransaction.conversionSignatures.head
        val wrongSig: Array[Byte] = (headSigs._2.head.bytes.head + 1).toByte +: headSigs._2.head.bytes.tail
        //noinspection ScalaStyle
        val wrongSigs: Map[(String, PublicKey25519Proposition), IndexedSeq[Signature25519]] =
          conversionTransaction.conversionSignatures +
            (headSigs._1 -> (conversionTransaction.conversionSignatures(headSigs._1).tail :+ Signature25519(wrongSig)))

        val invalidAR = conversionTransaction.copy(conversionSignatures = wrongSigs)

        val preExistingAssetBoxes: Set[BifrostBox] = conversionTransaction.totalAssetBoxes
          .flatMap {
            case (assetHub, propNonce) =>
              propNonce.map(r => AssetBox(
                r._1,
                r._2,
                conversionTransaction
                  .assetsToReturn(assetHub)
                  .map(_._2)
                  .sum +
                  conversionTransaction
                    .assetTokensToRedeem(assetHub)
                    .map(_._2)
                    .sum,
                assetHub._1,
                assetHub._2,
                conversionTransaction.data))
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingAssetBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(13))
          .get

        val newState = preparedState.validate(invalidAR)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Incorrect unlocker"
    }
  }

  property("Attempting to validate a ConversionTransaction with insufficient assets should error") {
    forAll(validConversionTxGen) {
      conversionTransaction: ConversionTransaction =>

        val preExistingAssetBoxes: Set[BifrostBox] = conversionTransaction
          .totalAssetBoxes
          .flatMap {
            case (assetHub, propNonce) => propNonce.map(r => AssetBox(r._1, r._2, 0, assetHub._1, assetHub._2, conversionTransaction.data))
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingAssetBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(14))
          .get

        val newState = preparedState.validate(conversionTransaction)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        //newState.failed.get.printStackTrace()
        newState.failed.get.getMessage shouldBe "Not enough assets"
    }
  }*/
}
