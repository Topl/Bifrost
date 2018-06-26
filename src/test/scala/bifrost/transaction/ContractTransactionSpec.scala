package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */

import bifrost.contract.{Agreement, Contract}
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.Role.Role
import bifrost.transaction.box.{ContractBox, ReputationBox}
import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
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

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes).map { _ =>
      positiveLongGen.sample.get -> (positiveLongGen.sample.get / 1e5.toLong + 1L)
    }

    val investmentBoxIds: IndexedSeq[Array[Byte]] = preInvestmentBoxes
      .map(n => PublicKeyNoncedBox.idFromBox(parties(0), n._1))

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

    val roles = IndexedSeq(Role.Investor, Role.Producer, Role.Hub)

    val messageToSign = Bytes.concat(
      AgreementCompanion.toBytes(agreement),
      roles.zip(parties).sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes),
      (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).reduce(_ ++ _))

    val signatures = allKeyPairs.map {
      keypair =>
        val sig = PrivateKey25519Companion.sign(keypair._1, messageToSign)
        (keypair._2, sig)
    }

    ContractCreation(
      agreement,
      preInvestmentBoxes,
      parties.zip(roles).toMap,
      signatures.toMap,
      feePreBoxes,
      fees,
      timestamp)
  }

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

    val gen: Gen[Agreement] = validAgreementGen(timestamp - effDelta, timestamp + expDelta)
    val validAgreement: Agreement = sampleUntilNonEmpty(gen)
    val contractBox: ContractBox = createContractBox(validAgreement, roles.zip(parties))

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
      contractBox.id
        ++ methodName.getBytes
        ++ sender._2._2.pubKeyBytes
        ++ parameters.noSpaces.getBytes
        ++ (contractBox.id ++ feeBoxIdKeyPairs.flatMap(_._1))
        ++ Longs.toByteArray(timestamp)
        ++ fees.flatMap { case (prop, feeValue) => prop.pubKeyBytes ++ Longs.toByteArray(feeValue) })

    val messageToSign = FastCryptographicHash(contractBox.value.noSpaces.getBytes ++ hashNoNonces)
    val signature = PrivateKey25519Companion.sign(sender._2._1, messageToSign)

    ContractMethodExecution(
      contractBox,
      methodName,
      parameters,
      Map(sender._2._2 -> sender._1),
      Map(sender._2._2 -> signature),
      feePreBoxes,
      fees,
      timestamp
    )
  }

  def potentiallyInvalidContractCompletionGen(minFee: Long,
                                              maxFee: Long,
                                              minFeeSum: Long,
                                              maxFeeSum: Long): Gen[ContractCompletion] = for {
    timestamp <- positiveLongGen
    agreement <- validAgreementGen()
    status <- Gen.oneOf(validStatuses)
    deliveredQuantity <- positiveLongGen
    numReputation <- positiveTinyIntGen
  } yield {
    /* 2*maxFee > 0 checks if 3*maxFee would overflow twice or not, same for minFee (underflow) */
    if ((minFeeSum > 3 * maxFee && 2 * maxFee > 0) || (maxFeeSum < 3 * minFee && 2 * minFee < 0) || minFeeSum > maxFeeSum || maxFee < minFee) {
      throw new Exception("Fee bounds are irreconciliable")
    }

    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

    val currentFulfillment = Map("deliveredQuantity" -> deliveredQuantity.asJson)
    val currentEndorsement = parties.map(p =>
                                           Base58.encode(p.pubKeyBytes) -> Base58.encode(FastCryptographicHash(
                                             currentFulfillment.asJson.noSpaces.getBytes)).asJson
    ).toMap

    val contractBox = createContractBox(agreement, roles.zip(parties))

    val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

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

    val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq
      .flatMap { case (prop, v) =>
        v.map {
          case (nonce, value) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
        }
      }
    val reasonableDoubleGen: Gen[Double] = Gen.choose(-1e3, 1e3)

    val reputation = (0 until numReputation)
      .map(_ =>
             ReputationBox(
               parties(roles.indexOf(Role.Producer)),
               Gen.choose(Long.MinValue, Long.MaxValue).sample.get,
               (reasonableDoubleGen.sample.get, reasonableDoubleGen.sample.get)))

    val boxIdsToOpen = IndexedSeq(contractBox.id) ++ reputation.map(_.id) ++ feeBoxIdKeyPairs.map(_._1)
    val fees = feePreBoxes.map { case (prop, preBoxes) =>
      val available = preBoxes.map(_._2).sum
      prop -> available
    }

    val messageToSign = FastCryptographicHash(
      contractBox.id ++
        roles.zip(parties).sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes) ++
        boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
        Longs.toByteArray(contract.lastUpdated) ++
        fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )

    ContractCompletion(
      contractBox,
      parties.zip(roles).toMap,
      parties.zip(signatures).toMap,
      feePreBoxes,
      fees,
      timestamp)

  }

  def potentiallyInvalidContractTransactionGen(minFee: Long = 0,
                                               maxFee: Long = Long.MaxValue,
                                               minFeeSum: Long = 0,
                                               maxFeeSum: Long = Long.MaxValue): Gen[ContractTransaction] = for {
    txType <- Gen.oneOf(ContractCreation, ContractCompletion, ContractMethodExecution)
  } yield {
    val typeGen: Gen[ContractTransaction] = txType match {
      case ContractCreation => potentiallyInvalidContractCreationGen(minFee, maxFee, minFeeSum, maxFeeSum)
      case ContractCompletion => potentiallyInvalidContractCompletionGen(minFee, maxFee, minFeeSum, maxFeeSum)
      case ContractMethodExecution => potentiallyInvalidContractMethodExecutionGen(minFee, maxFee, minFeeSum, maxFeeSum)
    }

    sampleUntilNonEmpty(typeGen)
  }

  property("ContractTransaction with any negative fee will error on semantic validation") {
    forAll(potentiallyInvalidContractTransactionGen(minFee = Long.MinValue, maxFee = Long.MaxValue)
             .suchThat(_.fees.exists(_._2 < 0))) {
      contractTransaction: ContractTransaction =>

        val tryValidate = contractTransaction match {
          case cc: ContractCreation => ContractCreation.validate(cc)
          case ccomp: ContractCompletion => ContractCompletion.validate(ccomp)
          case cm: ContractMethodExecution => ContractMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe "requirement failed: There was a negative fee"
    }
  }

  property("ContractTransaction which has fees summing to negative (overflow) " +
             "will error on semantic validation") {
    forAll(potentiallyInvalidContractTransactionGen(minFeeSum = Long.MinValue, maxFeeSum = -1L)
             .suchThat(_.fees.forall(_._2 >= 0))) {
      contractTransaction: ContractTransaction =>

        val tryValidate = contractTransaction match {
          case cc: ContractCreation => ContractCreation.validate(cc)
          case ccomp: ContractCompletion => ContractCompletion.validate(ccomp)
          case cm: ContractMethodExecution => ContractMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe "requirement failed: Fees did not sum to a positive value"
    }
  }

  property("ContractTransaction with any negative preFeeBox or summing to negative " +
             "will error on semantic validation") {
    forAll(potentiallyInvalidContractTransactionGen()
             .suchThat(tx =>
                         tx.preFeeBoxes.exists(seq => seq._2.exists(pb => pb._2 < 0))
                           || tx.preFeeBoxes.map(_._2.map(_._2).sum).exists(_ < 0L))) {
      contractTransaction: ContractTransaction =>

        val tryValidate = contractTransaction match {
          case cc: ContractCreation => ContractCreation.validate(cc)
          case ccomp: ContractCompletion => ContractCompletion.validate(ccomp)
          case cm: ContractMethodExecution => ContractMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe
          "requirement failed: There were negative polys provided or the sum was negative"
    }
  }


  property(
    "ContractTransaction which has preFeeBoxes summing to less than fee amounts " +
      "will error on semantic validation") {

  }

  property("ContractTransaction which has negative timestamp " +
             "will error on semantic validation") {

  }


}
