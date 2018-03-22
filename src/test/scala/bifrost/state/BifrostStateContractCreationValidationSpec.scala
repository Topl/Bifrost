package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.{AgreementCompanion, ContractCreation, Role}
import bifrost.transaction.box._
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random}

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class BifrostStateContractCreationValidationSpec extends BifrostStateSpec {

  def arbitraryPartyContractCreationGen(num: Int): Gen[ContractCreation] = for {
    agreement <- validAgreementGen()
    timestamp <- positiveLongGen
    numFeeBoxes <- positiveTinyIntGen
    numInvestmentBoxes <- positiveTinyIntGen
  } yield {
    val allKeyPairs = (0 until num).map(_ => keyPairSetGen.sample.get.head)

    val roles = Role.Investor +: Random.shuffle(List(Role.Producer, Role.Hub))
    val parties = (allKeyPairs.map(_._2) zip (Stream continually roles).flatten).map(t => t._2 -> t._1)

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get }
    val investmentBoxIds: IndexedSeq[Array[Byte]] = preInvestmentBoxes.map(n => PublicKeyNoncedBox.idFromBox(parties.head._2, n._1)) // TODO(balinskia): Which party is the investor

    val feePreBoxes = parties.map(_._2 -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get} ).toMap
    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq.flatMap { case (prop, v) =>
      v.map {
        case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
      }
    }

    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      prop -> (preBoxes.map(_._2).sum - Gen.choose(0L, Math.max(0, Math.min(Long.MaxValue, preBoxes.map(_._2).sum))).sample.get)
    }

    val messageToSign = Bytes.concat(
      AgreementCompanion.toBytes(agreement),
      parties.sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes),
      (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _)
    )

    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )

    ContractCreation(
      agreement,
      preInvestmentBoxes,
      parties,
      allKeyPairs.map(_._2).zip(signatures).toMap,
      feePreBoxes,
      fees,
      timestamp
    )
  }

  property("A block with valid contract creation will result in a contract entry and updated poly boxes in the LSMStore") {
    // Create block with contract creation
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc)
        )

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet // TODO(balinskia): Which party is the investor

        val box = cc.newBoxes.head.asInstanceOf[ContractBox]
        val returnedPolyBoxes: Traversable[PolyBox] = cc.newBoxes.tail.map {
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
        val newState = preparedState.applyChanges(preparedState.changes(block).get, Ints.toByteArray(2)).get

        require(newState.storage.get(ByteArrayWrapper(box.id)) match {
          case Some(wrapper) => wrapper.data sameElements boxBytes
          case None => false
        })

        require(returnedPolyBoxes.forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
          case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
          case None => false
        }))

        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        returnedPolyBoxes.map(_.value).sum shouldEqual
          preExistingPolyBoxes.map { case pb: PolyBox => pb.value }.sum - BigInt((cc.agreement.core.state \\ "initialCapital").head.as[String].right.get).toLong - cc.fee

        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        cc.fees.foreach { case (prop, fee) =>
          var isInvestor = 1L // 0L;

          //if(prop == cc.parties(Investor)) isInvestor = 1L // TODO(balinskia): Which party is the investor

          val output = (returnedPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop => pb.value }).sum

          val input = (preExistingPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop => pb.value }).sum
          val investment =
            if (prop equals cc.parties.head._2)  BigInt((cc.agreement.core.state \\ "initialCapital").head.as[String].right.get).toLong // TODO(balinskia): Which party is the investor
            else 0

          output shouldEqual (input - fee - investment)

        }

        /* Expect none of the prexisting boxes to still be around */
        preExistingPolyBoxes.foreach(pb => newState.storage.get(ByteArrayWrapper(pb.id)) shouldBe empty)

        BifrostStateSpec.genesisState = newState.rollbackTo(BifrostStateSpec.genesisBlockId).get
    }
  }

  property("Attempting to validate a contract creation tx without valid signatures should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>

        val wrongSig: Array[Byte] = (cc.signatures.head._2.bytes.head + 1).toByte +: cc.signatures.head._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = cc.signatures + (cc.signatures.head._1 -> Signature25519(wrongSig))
        val invalidCC = cc.copy(signatures = wrongSigs)

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet // TODO(balinskia): Which party is the investor

        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(invalidCC)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Not all roles were fulfilled for this transaction. Either they weren't provided or the signatures were not valid."
    }
  }

  property("Attempting to validate a contract creation tx without all roles should error") {
    forAll(arbitraryPartyContractCreationGen(Gen.choose(1, 2).sample.get)) {
      cc: ContractCreation =>

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet // TODO(balinskia): Which party is the investor

        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Not all roles were fulfilled for this transaction. Either they weren't provided or the signatures were not valid."
    }
  }

  //noinspection ScalaStyle
  property("Attempting to validate a contract creation tx with too many signatures (versus parties) should error") {
    forAll(arbitraryPartyContractCreationGen(Gen.choose(4, 10).sample.get)) {
      cc: ContractCreation =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet // TODO(balinskia): Which party is the investor

        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cc.signatures.keySet -- cc.parties.map(_._2).toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "Too many signatures for the parties of this transaction"
    }
  }

  property("Attempting to validate a contract creation tx with a timestamp that is before the last block timestamp should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet // TODO(balinskia): Which party is the investor

        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cc.signatures.keySet -- cc.parties.map(_._2).toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          cc.timestamp + Gen.choose(1L, Long.MaxValue - cc.timestamp - 1L).sample.get
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCreation attempts to write into the past"
    }
  }

  property("Attempting to validate a contract creation tx with the same id as an existing contract should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet  // TODO(balinskia): Which party is the investor

        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet ++
          ((cc.signatures.keySet -- cc.parties.map(_._2).toSet) zip (Stream continually roles).flatten).map(t =>
            ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role")
          )

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          cc.timestamp
        )

        val firstCCAddBlock = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc)
        )

        val necessaryState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get

        val preparedChanges = necessaryState.changes(firstCCAddBlock).get
        val preparedState = necessaryState.applyChanges(preparedChanges, Ints.toByteArray(2)).get.applyChanges(necessaryBoxesSC, Ints.toByteArray(3)).get

        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCreation attempts to overwrite existing contract"
    }
  }

  property("Attempting to validate a contract creation tx with a timestamp too far in the future should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>

        val preExistingPolyBoxes: Set[BifrostBox] = (cc.preFeeBoxes.flatMap { case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
        } ++ cc.preInvestmentBoxes.map(b => PolyBox(cc.parties.head._2, b._1, b._2))).toSet // TODO(balinskia): Which party is the investor

        val profileBoxes: Set[ProfileBox] = cc.parties.map {
          case (r: Role.Role, p: PublicKey25519Proposition) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
        }.toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli
        )

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(1)).get
        val newState = preparedState.validate(
          cc.copy(timestamp = Instant.now.toEpochMilli + Gen.choose(10L, 1000000L).sample.get)
        )

        BifrostStateSpec.genesisState = preparedState.rollbackTo(BifrostStateSpec.genesisBlockId).get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCreation timestamp is too far into the future"
    }
  }
}
