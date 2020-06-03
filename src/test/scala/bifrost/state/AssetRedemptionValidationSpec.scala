//package bifrost.state
//
//import java.time.Instant
//
//import bifrost.blocks.Block
//import bifrost.modifier.transaction.bifrostTransaction.AssetRedemption
//import bifrost.modifier.box._
//import com.google.common.primitives.Ints
//import io.iohk.iodb.ByteArrayWrapper
//import bifrost.modifier.box.proposition.PublicKey25519Proposition
//import bifrost.crypto.Signature25519
//import scorex.crypto.signatures.Curve25519
//
//import scala.util.Failure
//
///**
//  * Created by Matt Kindy on 6/7/2017.
//  */
//class AssetRedemptionValidationSpec extends BifrostStateSpec {
//
//  property("A block with valid AssetRedemption should result in the appropriate amount of tokens") {
//    forAll(validAssetRedemptionGen) {
//      ar: AssetRedemption =>
//        val block = Block(
//          Array.fill(Block.signatureLength)(-1: Byte),
//          Instant.now.toEpochMilli,
//          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
//          Signature25519(Array.fill(Block.signatureLength)(0: Byte)),
//          Seq(ar),
//          10L,
//          settings.version
//        )
//
//        val preExistingAssetBoxes: Set[Box] = ar
//          .availableToRedeem
//          .flatMap {
//            case (assetCode, toRedeem) =>
//              toRedeem
//                .map(r => AssetBox(r._1, r._2, ar.remainderAllocations(assetCode).map(_._2).sum, assetCode, ar.issuer, ar.data))
//          }
//          .toSet
//
//
//        val assetBoxes: Traversable[AssetBox] = ar.newBoxes.map {
//          case a: AssetBox => a
//          case _ => throw new Exception("Was expecting AssetBoxes but found something else")
//        }
//
//        val necessaryBoxesSC = BifrostStateChanges(
//          Set(),
//          preExistingAssetBoxes,
//          Instant.now.toEpochMilli
//        )
//
//        val preparedState = BifrostStateSpec
//          .genesisState
//          .applyChanges(necessaryBoxesSC, Ints.toByteArray(3))
//          .get
//
//        val newState = preparedState
//          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(4))
//          .get
//
//        ar.newBoxes
//          .forall(b => newState.storage.get(ByteArrayWrapper(b.id)) match {
//            case Some(wrapper) => wrapper.data sameElements b.bytes
//            case None => false
//          })
//
//        /* Expect none of the prexisting boxes to still be around */
//        require(preExistingAssetBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)).isEmpty))
//
//        BifrostStateSpec.genesisState = newState
//          .rollbackTo(BifrostStateSpec.genesisBlockId)
//          .get
//
//    }
//  }
//
//  property("Attempting to validate an AssetRedemption with a bad signature should error") {
//    forAll(validAssetRedemptionGen) {
//      assetRedemption: AssetRedemption =>
//
//        val headSigs = assetRedemption.signatures.head
//        val wrongSig: Array[Byte] = (headSigs._2.head.bytes.head + 1).toByte +: headSigs._2.head.bytes.tail
//        val wrongSigs: Map[String, IndexedSeq[Signature25519]] =
//          assetRedemption.signatures +
//            (headSigs._1 -> (assetRedemption.signatures(headSigs._1).tail :+ Signature25519(wrongSig)))
//
//        val invalidAR = assetRedemption.copy(signatures = wrongSigs)
//
//        val preExistingAssetBoxes: Set[Box] = assetRedemption
//          .availableToRedeem
//          .flatMap {
//            case (assetCode, toRedeem) =>
//              toRedeem.map(r => AssetBox(r._1,
//                                         r._2,
//                                         assetRedemption.remainderAllocations(assetCode).map(_._2).sum,
//                                         assetCode,
//                                         assetRedemption.issuer,
//                                         assetRedemption.data))
//          }
//          .toSet
//
//        val necessaryBoxesSC = BifrostStateChanges(
//          Set(),
//          preExistingAssetBoxes,
//          Instant.now.toEpochMilli
//        )
//
//        val preparedState = BifrostStateSpec
//          .genesisState
//          .applyChanges(necessaryBoxesSC, Ints.toByteArray(5))
//          .get
//
//        val newState = preparedState.validate(invalidAR)
//
//        BifrostStateSpec.genesisState = preparedState
//          .rollbackTo(BifrostStateSpec.genesisBlockId)
//          .get
//
//        newState shouldBe a[Failure[_]]
//        newState.failed.get.getMessage shouldBe "Incorrect unlocker"
//    }
//  }
//
//  property("Attempting to validate an AssetRedemption with insufficient assets should error") {
//    forAll(validAssetRedemptionGen) {
//      ar: AssetRedemption =>
//
//        val preExistingAssetBoxes: Set[Box] = ar.availableToRedeem.flatMap {
//          case (assetCode, toRedeem) => toRedeem.map(r => AssetBox(r._1, r._2, 0, assetCode, ar.issuer, ar.data))
//        }.toSet
//
//        val necessaryBoxesSC = BifrostStateChanges(Set(), preExistingAssetBoxes, Instant.now.toEpochMilli)
//
//        val preparedState = BifrostStateSpec
//          .genesisState
//          .applyChanges(necessaryBoxesSC, Ints.toByteArray(6))
//          .get
//
//        val newState = preparedState.validate(ar)
//
//        BifrostStateSpec.genesisState = preparedState
//          .rollbackTo(BifrostStateSpec.genesisBlockId)
//          .get
//
//        newState shouldBe a[Failure[_]]
//        newState.failed.get.getMessage shouldBe "Not enough assets"
//    }
//  }
//}
