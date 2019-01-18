package bifrost.state

import java.time.Instant

import bifrost.blocks.BifrostBlock
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.box._
import bifrost.transaction.{AgreementCompanion, Role}
import com.google.common.primitives.{Bytes, Ints}
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction.ContractCreation
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.state.PrivateKey25519Companion
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random}

/**
  * Created by Matt Kindy on 6/7/2017.
  */
class BifrostStateContractCreationValidationSpec extends ContractSpec {

  //noinspection ScalaStyle
  def arbitraryPartyContractCreationGen(num: Int): Gen[ContractCreation] = for {
    agreement <- validAgreementGen()
    timestamp <- positiveLongGen
    numFeeBoxes <- positiveTinyIntGen
    numInvestmentBoxes <- positiveTinyIntGen
    data <- stringGen
  } yield {
    val allKeyPairs = (0 until num).map(_ => keyPairSetGen.sample.get.head)

    val roles = Random.shuffle(List(Role.Producer, Role.Hub))
    val parties = IndexedSeq(allKeyPairs.head._2 -> Role.Investor) ++ allKeyPairs.drop(1).map(_._2)
      .zip((Stream continually roles).flatten)
      .map(t => t._1 -> t._2)

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] =
      (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get }

    val allInvestorsSorted = parties.filter(_._2 == Role.Investor).toSeq.sortBy(_._1.pubKeyBytes.toString)

    val investmentBoxIds: IndexedSeq[Array[Byte]] = // TODO(balinskia): Which party is the investor
      preInvestmentBoxes.map(n => {
        PublicKeyNoncedBox.idFromBox(allInvestorsSorted.head._1, n._1)})

    val feePreBoxes = parties
      .map(_._1 -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get })
      .toMap

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] =
      feePreBoxes
        .toIndexedSeq
        .flatMap {
          case (prop, v) => v.map {
            case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
          }
        }

    val fees = feePreBoxes.map {
      case (prop, preBoxes) =>
        val providedFunds = preBoxes.map(_._2).sum
        val possibleFeeValue = providedFunds -
          Gen
            .choose(0L, boundedBy(providedFunds, 0, Long.MaxValue))
            .sample
            .get

        prop -> possibleFeeValue
    }

    val boxIdsToOpen: IndexedSeq[Array[Byte]] = investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)

    val messageToSign = Bytes.concat(
      AgreementCompanion.toBytes(agreement),
      parties.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes),
      //(investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _))
      boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _))

    val signatures = allKeyPairs.map(keypair => PrivateKey25519Companion.sign(keypair._1, messageToSign))

    ContractCreation(
      agreement,
      preInvestmentBoxes,
      parties.toMap,
      allKeyPairs.map(_._2).zip(signatures).toMap,
      feePreBoxes,
      fees,
      timestamp,
      data)
  }

  private def boundedBy(sum: Long, min: Long, max: Long) = Math.max(min, Math.min(max, sum))

  property("A block with valid contract creation will result " +
             "in a contract entry and updated poly boxes in the LSMStore") {
    // Create block with contract creation
    forAll(validContractCreationGen) {
      contractCreation: ContractCreation =>
        val block = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(contractCreation),
          10L
        )

        // TODO(balinskia): Which party is the investor
        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(contractCreation)

        val box = contractCreation.newBoxes.head.asInstanceOf[ContractBox]
        val returnedPolyBoxes: Traversable[PolyBox] = contractCreation.newBoxes.tail.map {
          case p: PolyBox => p
          case _ => throw new Exception("Was expecting PolyBoxes but found something else")
        }

        val boxBytes = ContractBoxSerializer.toBytes(box)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(23))
          .get

        val newState = preparedState
          .applyChanges(preparedState.changes(block).get, Ints.toByteArray(24))
          .get

        require(newState.storage.get(ByteArrayWrapper(box.id))
                match {
                  case Some(wrapper) => wrapper.data sameElements boxBytes
                  case None => false
                })

        require(returnedPolyBoxes
                  .forall(pb => newState.storage.get(ByteArrayWrapper(pb.id)) match {
                    case Some(wrapper) => wrapper.data sameElements PolyBoxSerializer.toBytes(pb)
                    case None => false
                  }))


        /* Checks that the total sum of polys returned is total amount submitted minus total fees */
        returnedPolyBoxes.map(_.value).sum shouldEqual
          preExistingPolyBoxes
            .map { case pb: PolyBox => pb.value }
            .sum - BigInt((contractCreation.agreement.core.state \\ "initialCapital").head.as[String].right.get)
            .toLong - contractCreation.fee


        /* Checks that the amount returned in polys is equal to amount sent in less fees */
        contractCreation.fees.foreach { case (prop, fee) =>

          var isInvestor = 0L // 0L;

          if(prop == contractCreation.parties.head._1) isInvestor = 1L // TODO(balinskia): Which party is the investor)

          val output = (returnedPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop => pb.value }).sum

          val input = (preExistingPolyBoxes collect { case pb: PolyBox if pb.proposition equals prop =>
            pb.value }).sum

          val investment =
            if (prop equals contractCreation.parties.head._1) {
              BigInt((contractCreation.agreement.core.state \\ "initialCapital")
                       .head
                       .as[String]
                       .right
                       .get)
                .toLong
              // TODO(balinskia): Which party is the investor
            } else {
              0
            }


          output shouldEqual (input - fee - investment)
        }


        /* Expect none of the prexisting boxes to still be around */
        preExistingPolyBoxes
          .foreach(pb => newState.storage.get(ByteArrayWrapper(pb.id)) shouldBe empty)

        BifrostStateSpec.genesisState = newState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get
    }
  }

  property("Attempting to validate a contract creation tx without valid signatures should error") {
    forAll(validContractCreationGen) {
      contractCreation: ContractCreation =>

        val wrongSig: Array[Byte] = (contractCreation.signatures.head._2.bytes.head + 1).toByte +:
          contractCreation.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = contractCreation.signatures +
          (contractCreation.signatures.head._1 -> Signature25519(wrongSig))

        val invalidCC = contractCreation.copy(signatures = wrongSigs)

        // TODO(balinskia): Which party is the investor
        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(contractCreation)

        val profileBoxes: Set[ProfileBox] = contractCreation
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(25))
          .get

        val newState = preparedState.validate(invalidCC)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]

        newState.failed.get.getMessage shouldBe
          "Not all roles were fulfilled for this transaction. " +
            "Either they weren't provided or the signatures were not valid."
    }
  }

  property("Attempting to validate a contract creation tx without all roles should error") {
    forAll(arbitraryPartyContractCreationGen(Gen.choose(1, 2).sample.get)) {
      cc: ContractCreation =>

        // TODO(balinskia): Which party is the investor
        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(cc)

        val profileBoxes: Set[ProfileBox] = cc
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(26))
          .get

        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]

        newState.failed.get.getMessage shouldBe
          "Not all roles were fulfilled for this transaction. " +
            "Either they weren't provided or the signatures were not valid."
    }
  }

  //noinspection ScalaStyle
//  property("Attempting to validate a contract creation tx with too many signatures (versus parties) should error") {
//
//    forAll(arbitraryPartyContractCreationGen(Gen.choose(4, 10).sample.get)) {
//      cc: ContractCreation =>
//        val roles = Role.Investor +: Random.shuffle(List(Role.Producer, Role.Hub))
//
//        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(cc) // TODO(balinskia): Which party is the investor
//      val profileBoxes: Set[ProfileBox] = constructProfileBoxes(cc, roles)
//
//        val necessaryBoxesSC = BifrostStateChanges(
//          Set(),
//          preExistingPolyBoxes ++ profileBoxes,
//          Instant.now.toEpochMilli)
//
//        val preparedState = BifrostStateSpec
//          .genesisState
//          .applyChanges(necessaryBoxesSC, Ints.toByteArray(2))
//          .get
//
//        val newState = preparedState.validate(cc)
//
//        BifrostStateSpec.genesisState = preparedState
//          .rollbackTo(BifrostStateSpec.genesisBlockId)
//          .get
//
//        newState shouldBe a[Failure[_]]
//        newState.failed.get.getMessage shouldBe "Too many signatures for the parties of this transaction"
//    }
//  }

  property(
    "Attempting to validate a contract creation tx with a timestamp that is before the last block timestamp should error")
  {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(cc)
        val profileBoxes: Set[ProfileBox] = constructProfileBoxes(cc, roles)

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          cc.timestamp + Gen.choose(1L, Long.MaxValue - cc.timestamp - 1L).sample.get)

        val preparedState = BifrostStateSpec
          .genesisState.
          applyChanges(necessaryBoxesSC, Ints.toByteArray(28))
          .get

        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCreation attempts to write into the past"
    }
  }

  property("Attempting to validate a contract creation tx " +
             "with the same id as an existing contract should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(cc)
        val profileBoxes: Set[ProfileBox] = constructProfileBoxes(cc, roles)

        val necessaryBoxesSC = BifrostStateChanges(Set(), preExistingPolyBoxes ++ profileBoxes, cc.timestamp)

        val firstCCAddBlock = BifrostBlock(
          Array.fill(BifrostBlock.SignatureLength)(1: Byte),
          Instant.now.toEpochMilli,
          ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
          Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
          Seq(cc),
          10L
        )

        val necessaryState = BifrostStateSpec
          .genesisState
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(29))
          .get

        val preparedChanges = necessaryState.changes(firstCCAddBlock).get
        val preparedState = necessaryState
          .applyChanges(preparedChanges, Ints.toByteArray(30))
          .get
          .applyChanges(necessaryBoxesSC, Ints.toByteArray(31))
          .get

        val newState = preparedState.validate(cc)

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCreation attempts to overwrite existing contract"
    }
  }

  property("Attempting to validate a contract creation tx " +
             "with a timestamp too far in the future should error") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>

        val preExistingPolyBoxes: Set[BifrostBox] = getPreExistingPolyBoxes(cc)

        val profileBoxes: Set[ProfileBox] = cc
          .parties
          .map {
            case (p: PublicKey25519Proposition, r: Role.Role) =>
              ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
          }
          .toSet

        val necessaryBoxesSC = BifrostStateChanges(
          Set(),
          preExistingPolyBoxes ++ profileBoxes,
          Instant.now.toEpochMilli)

        val preparedState = BifrostStateSpec.genesisState.applyChanges(necessaryBoxesSC, Ints.toByteArray(32)).get
        val randomFutureTimestamp = Instant.now.toEpochMilli + Gen.choose(10L, 1000000L).sample.get
        val newState = preparedState.validate(cc.copy(timestamp = randomFutureTimestamp))

        BifrostStateSpec.genesisState = preparedState
          .rollbackTo(BifrostStateSpec.genesisBlockId)
          .get

        newState shouldBe a[Failure[_]]
        newState.failed.get.getMessage shouldBe "ContractCreation timestamp is too far into the future"
    }
  }
}
