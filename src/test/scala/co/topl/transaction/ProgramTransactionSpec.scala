package co.topl.transaction

/**
  * Created by cykoz on 5/11/2017.
  */

import co.topl.crypto.{FastCryptographicHash, PrivateKey25519}
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.modifier.transaction.{ProgramCreation, ProgramMethodExecution, ProgramTransaction}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{BoxId, CodeBox, ExecutionBox, PublicKeyNoncedBox, StateBox }
import co.topl.program.ExecutionBuilderSerializer
import co.topl.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.{Bytes, Longs}
import io.circe.syntax._
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.immutable.Seq

class ProgramTransactionSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  //noinspection ScalaStyle
  def potentiallyInvalidProgramCreationGen(minFee: Long,
                                            maxFee: Long,
                                            minFeeSum: Long,
                                            maxFeeSum: Long): Gen[ProgramCreation] = for {
    executionBuilder <- validExecutionBuilderGen()
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

    val keyPair = keyPairSetGen.sample.get.head
    val sender = keyPair._2

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numInvestmentBoxes).map { _ =>
      positiveLongGen.sample.get -> (positiveLongGen.sample.get / 1e5.toLong + 1L)
    }

    val investmentBoxIds: IndexedSeq[BoxId] = preInvestmentBoxes
      .map(n => PublicKeyNoncedBox.idFromBox(sender, n._1))

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = {
      Map(sender -> IndexedSeq(preFeeBoxGen(minFee, maxFee).sample.get))
    }

    val feeBoxIdKeyPairs: IndexedSeq[(BoxId, PublicKey25519Proposition)] = feePreBoxes.toIndexedSeq
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

    val messageToSign = Bytes.concat(
      ExecutionBuilderSerializer.toBytes(executionBuilder),
      //roles.zip(parties).sortBy(_._1).foldLeft(Array[Byte]())((a, b) => a ++ b._2.pubKeyBytes),
      keyPair._2.pubKeyBytes,
      (investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)).foldLeft(Array[Byte]())((acc, box) => acc ++ box.hashBytes),
      data.getBytes)

    val signatures = Map(sender -> keyPair._1.sign(messageToSign))


    val stateTwo =
      s"""
         |{ "b": 0 }
         """.stripMargin.asJson

    val stateThree =
      s"""
         |{ "c": 0 }
         """.stripMargin.asJson

    val stateBoxTwo = StateBox(sender, 1L, null, stateTwo)
    val stateBoxThree = StateBox(sender, 2L, null, stateThree)

    val readOnlyIds = Seq(stateBoxTwo.value, stateBoxThree.value)

    ProgramCreation(
      executionBuilder,
      readOnlyIds,
      preInvestmentBoxes,
      sender,
      signatures,
      feePreBoxes,
      fees,
      timestamp,
      data)
  }

  //noinspection ScalaStyle
  def potentiallyInvalidProgramMethodExecutionGen(minFee: Long,
                                                   maxFee: Long,
                                                   minFeeSum: Long,
                                                   maxFeeSum: Long): Gen[ProgramMethodExecution] = for {
    methodName <- Gen.oneOf(validProgramMethods)
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

    val (priv: PrivateKey25519, sender: PublicKey25519Proposition) = keyPairSetGen.sample.get.head

    val state =
      s"""
         |{ "a": "0" }
       """.stripMargin.asJson

    val stateBox = StateBox(sender, 0L, programIdGen.sample.get, state)

    val codeBox = CodeBox(sender, 1L, programIdGen.sample.get, Seq("add = function() { a = 2 + 2 }"), Map("add" -> Seq("Number, Number")))

    val proposition = sender

    val executionBox = ExecutionBox(proposition, 2L, programIdGen.sample.get, Seq(stateBox.value), Seq(codeBox.value))

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] =
      Map(sender -> IndexedSeq(preFeeBoxGen(minFee, maxFee).sample.get))

    val feeBoxIdKeyPairs: IndexedSeq[(BoxId, PublicKey25519Proposition)] = feePreBoxes
      .toIndexedSeq
      .flatMap { case (prop, v) =>
        v.map {
          case (nonce, value) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
        }
      }

    val fees = feePreBoxes.map {
      case (prop, preBoxes) =>
        val available = preBoxes.map(_._2).sum
        prop -> available
    }

    val hashNoNonces = FastCryptographicHash(
      executionBox.id.hashBytes
        ++ methodName.getBytes
        ++ sender.pubKeyBytes
        ++ parameters.noSpaces.getBytes
        ++ (executionBox.id.hashBytes ++ feeBoxIdKeyPairs.flatMap(_._1.hashBytes))
        ++ Longs.toByteArray(timestamp)
        ++ fees.flatMap { case (prop, feeValue) => prop.pubKeyBytes ++ Longs.toByteArray(feeValue) })

    val messageToSign = Bytes.concat(
      FastCryptographicHash(executionBox.bytes ++ hashNoNonces),
        data.getBytes)
    val signature = priv.sign(messageToSign)

    ProgramMethodExecution(
      executionBox,
      Seq(stateBox),
      Seq(codeBox),
      methodName,
      parameters,
      sender,
      Map(sender -> signature),
      feePreBoxes,
      fees,
      timestamp,
      data
    )
  }

  def potentiallyInvalidProgramTransactionGen(minFee: Long = 0,
                                               maxFee: Long = Long.MaxValue,
                                               minFeeSum: Long = 0,
                                               maxFeeSum: Long = Long.MaxValue): Gen[ProgramTransaction] = for {
    txType <- Gen.oneOf(ProgramCreation, ProgramMethodExecution)
  } yield {
    val typeGen: Gen[ProgramTransaction] = txType match {
      case ProgramCreation => potentiallyInvalidProgramCreationGen(minFee, maxFee, minFeeSum, maxFeeSum)
      case ProgramMethodExecution => potentiallyInvalidProgramMethodExecutionGen(minFee, maxFee, minFeeSum, maxFeeSum)
    }

    sampleUntilNonEmpty(typeGen)
  }

  /*property("ProgramTransaction with any negative fee will error on semantic validation") {
    forAll(potentiallyInvalidProgramTransactionGen(minFee = Long.MinValue, maxFee = Long.MaxValue)
             .suchThat(_.fees.exists(_._2 < 0))) {
      programTransaction: ProgramTransaction =>

        val tryValidate = programTransaction match {
          case cc: ProgramCreation => ProgramCreation.validate(cc)
          case cm: ProgramMethodExecution => ProgramMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe "requirement failed: There was a negative fee"
    }
  }*/

  /*property("ProgramTransaction which has fees summing to negative (overflow) " +
             "will error on semantic validation") {
    forAll(potentiallyInvalidProgramTransactionGen(minFeeSum = Long.MinValue, maxFeeSum = -1L)
             .suchThat(_.fees.forall(_._2 >= 0))) {
      programTransaction: ProgramTransaction =>

        val tryValidate = programTransaction match {
          case cc: ProgramCreation => ProgramCreation.validate(cc)
          case cm: ProgramMethodExecution => ProgramMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe "requirement failed: Fees did not sum to a positive value"
    }
  }*/

  /*property("ProgramTransaction with any negative preFeeBox or summing to negative " +
             "will error on semantic validation") {
    forAll(potentiallyInvalidProgramTransactionGen()
             .suchThat(tx =>
                         tx.preFeeBoxes.exists(seq => seq._2.exists(pb => pb._2 < 0))
                           || tx.preFeeBoxes.map(_._2.map(_._2).sum).exists(_ < 0L))) {
      programTransaction: ProgramTransaction =>

        val tryValidate = programTransaction match {
          case cc: ProgramCreation => ProgramCreation.validate(cc)
          case cm: ProgramMethodExecution => ProgramMethodExecution.validate(cm)
        }

        tryValidate shouldBe a[Failure[_]]
        tryValidate.failed.get.getMessage shouldBe
          "requirement failed: There were negative polys provided or the sum was negative"
    }
  }*/


  /* property(
    "ProgramTransaction which has preFeeBoxes summing to less than fee amounts " +
      "will error on semantic validation") {

  } */

  /* property("ProgramTransaction which has negative timestamp " +
             "will error on semantic validation") {

  } */


}
