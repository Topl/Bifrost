package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */

import java.util.UUID

import bifrost.contract.{Contract, ExecutionBuilder}
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction.Role.Role
import bifrost.transaction.box.{CodeBox, ContractBox, ExecutionBox, ReputationBox, StateBox}
import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction._
import bifrost.transaction.box.proposition.{MofNProposition, PublicKey25519Proposition}
import bifrost.transaction.serialization.AgreementCompanion
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58

import scala.collection.immutable.Seq
import scala.util.{Failure, Random, Success}

class ContractTransactionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  //noinspection ScalaStyle
  def potentiallyInvalidContractCreationGen(minFee: Long,
                                            maxFee: Long,
                                            minFeeSum: Long,
                                            maxFeeSum: Long): Gen[ContractCreation] = for {
    agreement <- validAgreementGen()
    timestamp <- positiveLongGen
    numInvestmentBoxes <- positiveTinyIntGen
    data <- stringGen
  } yield {
    /* 2*maxFee > 0 checks if 3*maxFee would overflow twice or not, same for minFee (underflow) */
    if ((minFeeSum > 3 * maxFee && 2 * maxFee > 0)
      || (maxFeeSum < 3 * minFee && 2 * minFee < 0)
      || minFeeSum > maxFeeSum
      || maxFee < minFee) {
      throw new Exception("Fee bounds are irreconciliable")
    }

    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val roles = IndexedSeq(Role.Investor, Role.Producer, Role.Hub)
    val partiesWithRoles = parties.zip(roles)

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes).map { _ =>
      positiveLongGen.sample.get -> (positiveLongGen.sample.get / 1e5.toLong + 1L)
    }

    val investmentBoxIds: IndexedSeq[Array[Byte]] = preInvestmentBoxes
      .map(n => PublicKeyNoncedBox.idFromBox(partiesWithRoles.find(_._2 == Role.Investor).get._1, n._1))

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = {
      val sum = Gen.choose(minFeeSum, maxFeeSum).sample.get

      splitAmongN(sum, parties.length, minFee, maxFee) match {
        case Success(shares) => parties
          .zip(shares)
          .map {
            case (party, share) =>
              party -> (splitAmongN(share, positiveTinyIntGen.sample.get) match {
                case Success(boxAmounts) => boxAmounts
                case f: Failure[_] => throw f.exception
              })
                .map { boxAmount => preFeeBoxGen(boxAmount, boxAmount).sample.get }
                .toIndexedSeq
          }.toMap

        case f: Failure[_] => throw f.exception
      }
    }

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq
      .flatMap {
        case (prop, v) =>
          v.map {
            case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
          }
      }

    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      val available = preBoxes.map(_._2).sum
      prop -> available
    }

    //val roles = IndexedSeq(Role.Investor, Role.Producer, Role.Hub)

    val messageToSign = Bytes.concat(
      AgreementCompanion.toBytes(agreement),
      //roles.zip(parties).sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes),
      partiesWithRoles.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes),
      (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _),
      data.getBytes)

    val signatures = allKeyPairs.map {
      keypair =>
        val sig = PrivateKey25519Companion.sign(keypair._1, messageToSign)
        (keypair._2, sig)
    }

    ContractCreation(
      agreement,
      preInvestmentBoxes,
      partiesWithRoles.toMap,
      signatures.toMap,
      feePreBoxes,
      fees,
      timestamp,
      data)
  }

  //noinspection ScalaStyle
  def potentiallyInvalidContractMethodExecutionGen(minFee: Long,
                                                   maxFee: Long,
                                                   minFeeSum: Long,
                                                   maxFeeSum: Long): Gen[ContractMethodExecution] = for {
    methodName <- Gen.oneOf(validContractMethods)
    parameters <- jsonGen()
    timestamp <- positiveLongGen.map(_ / 3)
    deliveredQuantity <- positiveLongGen
    effDelta <- positiveLongGen.map(_ / 3)
    expDelta <- positiveLongGen.map(_ / 3)
    data <- stringGen
  } yield {
    /* 2*maxFee > 0 checks if 3*maxFee would overflow twice or not, same for minFee (underflow) */
    if ((minFeeSum > 3 * maxFee && 2 * maxFee > 0)
      || (maxFeeSum < 3 * minFee && 2 * minFee < 0)
      || minFeeSum > maxFeeSum
      || maxFee < minFee) {
      throw new Exception("Fee bounds are irreconciliable")
    }

    val allKeyPairs: Seq[(PrivateKey25519, PublicKey25519Proposition)] =
      (0 until 3).map(_ => sampleUntilNonEmpty(keyPairSetGen).head)

    val parties: Seq[PublicKey25519Proposition] = allKeyPairs.map(_._2)
    val roles: Seq[Role] = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

    val gen: Gen[ExecutionBuilder] = validAgreementGen(timestamp - effDelta, timestamp + expDelta)
    val validAgreement: ExecutionBuilder = sampleUntilNonEmpty(gen)

    val stateBox = StateBox(parties.head, 0L, Seq("var a = 0"), true)

    val codeBox = CodeBox(parties.head, 1L, Seq("add = function() { a = 2 + 2 }"))

    val stateBoxUUID: UUID = UUID.nameUUIDFromBytes(stateBox.id)

    val proposition = MofNProposition(1, parties.map(_.pubKeyBytes).toSet)

    val executionBox = ExecutionBox(proposition, 2L, Seq(stateBoxUUID), Seq(codeBox.id))

    val sender = Gen.oneOf(Seq(Role.Producer, Role.Investor, Role.Hub).zip(allKeyPairs)).sample.get

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = {
      val sum = Gen.choose(minFeeSum, maxFeeSum).sample.get

      splitAmongN(sum, parties.length, minFee, maxFee) match {
        case Success(shares) => parties.zip(shares).map { case (party, share) =>
          party -> (splitAmongN(share, positiveTinyIntGen.sample.get) match {
            case Success(boxAmounts) => boxAmounts
          }).map { boxAmount => preFeeBoxGen(boxAmount, boxAmount).sample.get }.toIndexedSeq
        }.toMap

        case f: Failure[_] => throw f.exception
      }
    }
    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes
      .toIndexedSeq
      .flatMap { case (prop, v) =>
        v.map {
          case (nonce, value) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
        }
      }

    val senderFeePreBoxes = feePreBoxes(sender._2._2)

    val fees = feePreBoxes.map {
      case (prop, preBoxes) =>
        val available = preBoxes.map(_._2).sum
        prop -> available
    }

    val hashNoNonces = FastCryptographicHash(
      executionBox.id
        ++ methodName.getBytes
        ++ sender._2._2.pubKeyBytes
        ++ parameters.noSpaces.getBytes
        ++ (executionBox.id ++ feeBoxIdKeyPairs.flatMap(_._1))
        ++ Longs.toByteArray(timestamp)
        ++ fees.flatMap { case (prop, feeValue) => prop.pubKeyBytes ++ Longs.toByteArray(feeValue) })

    val messageToSign = Bytes.concat(
      FastCryptographicHash(executionBox.bytes ++ hashNoNonces),
        data.getBytes)
    val signature = PrivateKey25519Companion.sign(sender._2._1, messageToSign)

    bifrostTransaction.ContractMethodExecution(
      stateBox,
      codeBox,
      executionBox,
      methodName,
      parameters,
      Map(sender._2._2 -> sender._1),
      Map(sender._2._2 -> signature),
      feePreBoxes,
      fees,
      timestamp,
      data
    )
  }

  def potentiallyInvalidContractTransactionGen(minFee: Long = 0,
                                               maxFee: Long = Long.MaxValue,
                                               minFeeSum: Long = 0,
                                               maxFeeSum: Long = Long.MaxValue): Gen[ContractTransaction] = for {
    txType <- Gen.oneOf(ContractCreation, ContractMethodExecution)
  } yield {
    val typeGen: Gen[ContractTransaction] = txType match {
      case ContractCreation => potentiallyInvalidContractCreationGen(minFee, maxFee, minFeeSum, maxFeeSum)
      case ContractMethodExecution => potentiallyInvalidContractMethodExecutionGen(minFee, maxFee, minFeeSum, maxFeeSum)
    }

    sampleUntilNonEmpty(typeGen)
  }

  /*property("ContractTransaction with any negative fee will error on semantic validation") {
    forAll(potentiallyInvalidContractTransactionGen(minFee = Long.MinValue, maxFee = Long.MaxValue)
             .suchThat(_.fees.exists(_._2 < 0))) {
      contractTransaction: ContractTransaction =>

        val tryValidate = contractTransaction match {
          case cc: ContractCreation => ContractCreation.validate(cc)
          case cm: ContractMethodExecution => ContractMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe "requirement failed: There was a negative fee"
    }
  }*/

  /*property("ContractTransaction which has fees summing to negative (overflow) " +
             "will error on semantic validation") {
    forAll(potentiallyInvalidContractTransactionGen(minFeeSum = Long.MinValue, maxFeeSum = -1L)
             .suchThat(_.fees.forall(_._2 >= 0))) {
      contractTransaction: ContractTransaction =>

        val tryValidate = contractTransaction match {
          case cc: ContractCreation => ContractCreation.validate(cc)
          case cm: ContractMethodExecution => ContractMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe "requirement failed: Fees did not sum to a positive value"
    }
  }*/

  /*property("ContractTransaction with any negative preFeeBox or summing to negative " +
             "will error on semantic validation") {
    forAll(potentiallyInvalidContractTransactionGen()
             .suchThat(tx =>
                         tx.preFeeBoxes.exists(seq => seq._2.exists(pb => pb._2 < 0))
                           || tx.preFeeBoxes.map(_._2.map(_._2).sum).exists(_ < 0L))) {
      contractTransaction: ContractTransaction =>

        val tryValidate = contractTransaction match {
          case cc: ContractCreation => ContractCreation.validate(cc)
          case cm: ContractMethodExecution => ContractMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe
          "requirement failed: There were negative polys provided or the sum was negative"
    }
  }*/


  property(
    "ContractTransaction which has preFeeBoxes summing to less than fee amounts " +
      "will error on semantic validation") {

  }

  property("ContractTransaction which has negative timestamp " +
             "will error on semantic validation") {

  }


}
