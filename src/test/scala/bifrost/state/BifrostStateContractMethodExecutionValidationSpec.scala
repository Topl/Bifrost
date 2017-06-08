package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.transaction.box._
import bifrost.transaction.{AgreementCompanion, ContractCreation, ContractMethodExecution, Role}
import com.google.common.primitives.{Bytes, Ints}
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random}

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class BifrostStateContractMethodExecutionValidationSpec extends BifrostStateSpec {

  property("A block with valid CME will result in a contract entry and updated poly boxes in the LSMStore") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cme)
        )

        val preExistingPolyBoxes: Set[BifrostBox] = cme.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val box = cme.newBoxes.head.asInstanceOf[ContractBox]
        val deductedFeeBoxes: Traversable[PolyBox] = cme.newBoxes.tail.map {
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
        }

        val boxBytes = ContractBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes + cme.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.applyChanges(preparedState.changes(block).get, Ints.toByteArray(2)).get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        require(deductedFeeBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
          case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
          case None => false
        }))

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        require(deductedFeeBoxes.map(_.value).sum == preExistingPolyBoxes.map { case pb: PolyBox => pb.value }.sum - cme.fee)

        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        require(cme.fees.forall(p => deductedFeeBoxes.filter(_.proposition equals p._1).map(_.value).sum == cme.feePreBoxes(p._1).map(_._2).sum - cme.fees(p._1)))

        /* Expect none of the prexisting boxes to still be around */
        require(preExistingPolyBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)).isEmpty))

        BifrostStateSpec.genesisState = newState.rollbackTo(BifrostStateSpec.genesisBlockId).get

    }
  }

  property("Attempting to validate a CME without valid signatures should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>

        val wrongSig: Array[Byte] = (cme.signatures.head._2.bytes.head + 1).toByte +: cme.signatures.head._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = cme.signatures + (cme.signatures.head._1 -> Signature25519(wrongSig))
        val invalidCME = cme.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[BifrostBox] = cme.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cme.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + cme.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(invalidCME)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Signature is invalid for contractBox"
    }
  }

  property("Attempting to validate a CME with a party that is not part of the contract should error") {

  }

  //noinspection ScalaStyle
  property("Attempting to validate a CME with too many signatures (versus parties) should error") {

  }

  property("Attempting to validate a CME with a timestamp that is before the last block timestamp should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>

        val preExistingPolyBoxes: Set[BifrostBox] = cme.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cme.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + cme.contractBox,
          cme.timestamp + Gen.choose(1L, Long.MaxValue - cme.timestamp - 1L).sample.get
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractMethodExecution attempts to write into the past"
    }
  }

  property("Attempting to validate a CME for a contract that doesn't exist should error") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cme)
        )

        val preExistingPolyBoxes: Set[BifrostBox] = cme.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val box = cme.newBoxes.head.asInstanceOf[ContractBox]
        val deductedFeeBoxes: Traversable[PolyBox] = cme.newBoxes.tail.map {
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
        }

        val boxBytes = ContractBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe s"Contract ${Base58.encode(cme.contractBox.id)} does not exist"
    }
  }

  property("Attempting to validate a CME with a timestamp too far in the future should error") {
    forAll(semanticallyValidContractMethodExecutionGen.suchThat(_.timestamp > Instant.now.toEpochMilli + 50L)) {
      cme: ContractMethodExecution =>
        val preExistingPolyBoxes: Set[BifrostBox] = cme.feePreBoxes.flatMap { case (prop, preBoxes) => preBoxes.map(b => PolyBox(prop, b._1, b._2)) }.toSet
        val profileBoxes: Set[ProfileBox] = cme.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes + cme.contractBox,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cme)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractMethodExecution timestamp is too far into the future"
    }
  }
}
