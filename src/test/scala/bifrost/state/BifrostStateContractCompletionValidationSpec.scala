package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.contract.Contract
import bifrost.transaction.box._
import bifrost.transaction.{ContractCompletion, Role}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.signatures.Curve25519
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.{Failure, Random}

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class BifrostStateContractCompletionValidationSpec extends BifrostStateSpec {

  def arbitraryPartyContractCompletionGen(num: Int): Gen[ContractCompletion] = for {
    timestamp <- positiveLongGen
    agreement <- validAgreementGen()
    status <- Gen.oneOf(validStatuses)
    deliveredQuantity <- positiveLongGen
    numReputation <- positiveTinyIntGen
    numFeeBoxes <- positiveTinyIntGen
  } yield {
    val allKeyPairs = (0 until num).map(_ => keyPairSetGen.sample.get.head)

    val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))
    val parties = (allKeyPairs.map(_._2) zip (Stream continually roles).flatten).map(t => t._2 -> t._1).toMap

    val currentFulfillment = Map("deliveredQuantity" -> deliveredQuantity.asJson)
    val currentEndorsement = Map[String, Json]()

    val contractBox = createContractBox(agreement, parties)

    val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

    val feePreBoxes = parties.map(_._2 -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get} )
    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq.flatMap { case (prop, v) =>
      v.map {
        case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
      }
    }

    val reasonableDoubleGen: Gen[Double] = Gen.choose(-1e3, 1e3)

    val reputation = (0 until numReputation).map(_ =>
      ReputationBox(parties(Role.Producer), Gen.choose(Long.MinValue, Long.MaxValue).sample.get, (reasonableDoubleGen.sample.get, reasonableDoubleGen.sample.get))
    )

    val boxIdsToOpen = IndexedSeq(contractBox.id) ++ reputation.map(_.id) ++ feeBoxIdKeyPairs.map(_._1)

    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      prop -> (preBoxes.map(_._2).sum - Gen.choose(0L, Math.max(0, Math.min(Long.MaxValue, preBoxes.map(_._2).sum))).sample.get)
    }

    val messageToSign = Bytes.concat(
      contractBox.id,
      parties.flatMap(_._2.pubKeyBytes).toArray,
      boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(contract.lastUpdated),
      fees.flatMap(f => f._1.pubKeyBytes ++ Longs.toByteArray(f._2)).toArray
    )

    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )

    ContractCompletion(
      contractBox,
      reputation,
      parties,
      allKeyPairs.map(_._2).zip(signatures).toMap,
      feePreBoxes,
      fees,
      timestamp
    )
  }

  property("A block with valid ContractCompletion will remove the contract entry and update poly boxes in the LSMStore") {
    // Create block with contract creation
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc)
        )

        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val preExistingReputationBoxes: Set[ReputationBox] = cc.producerReputation.toSet

        val box = cc.newBoxes.head.asInstanceOf[ReputationBox]
        val assetBoxes: Traversable[AssetBox] = cc.newBoxes.slice(1,3).map {
          case a: AssetBox => a
          case _ => throw new Exception("Was expecting AssetBoxes but found something else")
        }
        val deductedFeeBoxes: Traversable[PolyBox] = cc.newBoxes.slice(4, cc.newBoxes.size).map {
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
        }

        val boxBytes = ReputationBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingReputationBoxes ++ preExistingPolyBoxes + cc.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.applyChanges(preparedState.changes(block).get, Ints.toByteArray(2)).get

        require(newState.storage.get(ByteArrayWrapper(cc.contractBox.id)).isEmpty)

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        require(deductedFeeBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
          case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
          case None => false
        }))

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        require(deductedFeeBoxes.map(_.value).sum == preExistingPolyBoxes.map { case pb: PolyBox => pb.value }.sum - cc.fee)

        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        require(cc.fees.forall(p => deductedFeeBoxes.filter(_.proposition equals p._1).map(_.value).sum == cc.preFeeBoxes(p._1).map(_._2).sum - cc.fees(p._1)))

        /* Expect none of the prexisting boxes to still be around */
        require(preExistingPolyBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)).isEmpty))

        BifrostStateSpec.genesisState = newState.rollbackTo(BifrostStateSpec.genesisBlockId).get

    }
  }

  property("Attempting to validate a ContractCompletion without valid signatures should error") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>

        val wrongSig: Array[Byte] = (cc.signatures.head._2.bytes.head + 1).toByte +: cc.signatures.head._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = cc.signatures + (cc.signatures.head._1 -> Signature25519(wrongSig))
        val invalidCC = cc.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val preExistingReputationBoxes: Set[ReputationBox] = cc.producerReputation.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ preExistingReputationBoxes + cc.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(invalidCC)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Role does not exist"
    }
  }

  property("Attempting to validate a ContractCompletion without all roles should error") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>

        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val preExistingReputationBoxes: Set[ReputationBox] = cc.producerReputation.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ preExistingReputationBoxes ++ profileBoxes - profileBoxes.head + cc.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Role does not exist"
    }
  }

  //noinspection ScalaStyle
  property("Attempting to validate a ContractCompletion with too many signatures (versus parties) should error") {
    forAll(arbitraryPartyContractCompletionGen(Gen.choose(4, 10).sample.get)) {
      cc: ContractCompletion =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))
        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cc.signatures.keySet -- cc.parties.values.toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ preExistingPolyBoxes ++ profileBoxes + cc.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage should (be ("Signature is invalid for contractBox") or be ("Unexpected signature for this transaction"))
    }
  }

  property("Attempting to validate a ContractCompletion with a timestamp that is before the last block timestamp should error") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val preExistingReputationBoxes: Set[ReputationBox] = cc.producerReputation.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cc.signatures.keySet -- cc.parties.values.toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ preExistingPolyBoxes ++ profileBoxes + cc.contractBox,
          cc.timestamp + Gen.choose(1L, Long.MaxValue - cc.timestamp - 1L).sample.get
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCompletion attempts to write into the past"
    }
  }

  property("Attempting to validate a ContractCompletion with a contract that doesn't exist") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val preExistingReputationBoxes: Set[ReputationBox] = cc.producerReputation.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cc.signatures.keySet -- cc.parties.values.toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ preExistingReputationBoxes ++ profileBoxes,
          cc.timestamp
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Contract ${Base58.encode(cc.contractBox.id)} does not exist"
    }
  }

  property("Attempting to validate a ContractCompletion with a timestamp too far in the future should error") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val preExistingPolyBoxes: Set[BifrostBox] = cc.preFeeBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val preExistingReputationBoxes: Set[ReputationBox] = cc.producerReputation.toSet
        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ preExistingReputationBoxes ++ profileBoxes + cc.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(
          cc.copy(timestamp = Instant.now.toEpochMilli + Gen.choose(10L, 1000000L).sample.get)
        )

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCompletion timestamp is too far in the future"
    }
  }
}
