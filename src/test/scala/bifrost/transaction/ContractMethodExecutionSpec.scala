package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import java.time.Instant

import bifrost.contract.Agreement
import bifrost.state.BifrostState
import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.Longs
import io.circe.Json
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Random

class ContractMethodExecutionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ContractMethodExecution Tx should be valid") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>
        val semanticValid = BifrostState.semanticValidity(cme)
        semanticValid.isSuccess shouldBe true
    }
  }

  property("Tx with modified signature should be invalid") {
    forAll(semanticallyValidContractMethodExecutionGen) {
      cme: ContractMethodExecution =>
        val wrongSig: Array[Byte] = (cme.signatures.head._2.bytes.head + 1).toByte +: cme.signatures.head._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = cme.signatures + (cme.signatures.head._1 -> Signature25519(wrongSig))
        BifrostState.semanticValidity(cme.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

  property("Tx on contract before effective date should be invalid") {

    val preEffContractMethodExecutionGen = for {
      methodName <- Gen.oneOf(validContractMethods)
      parameters <- jsonArrayGen()
      fee <- positiveLongGen
      timestamp <- positiveLongGen
      numFeeBoxes <- positiveTinyIntGen
    } yield {
      val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
      val parties = allKeyPairs.map(_._2)
      val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

      val currentFulfillment = Map[String, Json]()

      val contractBox = createContractBox(
        Agreement(agreementTermsGen.sample.get, timestamp - 100000, timestamp - 100000 + Gen.choose(0, 2000).sample.get),
        "initialized",
        currentFulfillment,
        roles.zip(parties).toMap
      )

      val sender = Gen.oneOf(Seq(Role.Producer, Role.Investor , Role.Hub).zip(allKeyPairs)).sample.get

      val hashNoNonces = FastCryptographicHash(
        contractBox.id ++
          methodName.getBytes ++
          (sender._2)._2.pubKeyBytes ++
          parameters.noSpaces.getBytes ++
          contractBox.id ++
          Longs.toByteArray(timestamp) ++
          Longs.toByteArray(fee)
      )

      val messageToSign = FastCryptographicHash(contractBox.value.asObject.get("storage").get.noSpaces.getBytes ++ hashNoNonces)
      val signature = PrivateKey25519Companion.sign((sender._2)._1, messageToSign)

      ContractMethodExecution(
        contractBox,
        methodName,
        parameters,
        Map(sender._1 -> sender._2._2),
        Map(sender._2._2 -> signature),
        Map(sender._2._2 -> (0 until numFeeBoxes).map { _ => preFeeBoxGen.sample.get}),
        Map(sender._2._2 -> positiveTinyIntGen.sample.get.toLong),
        timestamp
      )
    }

    forAll(preEffContractMethodExecutionGen) {
      cme: ContractMethodExecution => {
        val semanticValid = BifrostState.semanticValidity(cme)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
      }
    }
  }

  property("Tx on contract after expiration date should be invalid") {

    val postExpContractMethodExecutionGen = for {
      methodName <- Gen.oneOf(validContractMethods)
      parameters <- jsonArrayGen()
      fee <- positiveLongGen
      timestamp <- positiveLongGen
      numFeeBoxes <- positiveTinyIntGen
    } yield {
      val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
      val parties = allKeyPairs.map(_._2)
      val roles = Random.shuffle(List(Role.Investor, Role.Producer, Role.Hub))

      val currentFulfillment = Map[String, Json]()

      val contractBox = createContractBox(
        Agreement(agreementTermsGen.sample.get, timestamp - 100000, timestamp - 100000 + Gen.choose(0, 2000).sample.get),
        "expired",
        currentFulfillment,
        roles.zip(parties).toMap
      )

      val sender = Gen.oneOf(Seq(Role.Producer, Role.Investor , Role.Hub).zip(allKeyPairs)).sample.get

      val hashNoNonces = FastCryptographicHash(
        contractBox.id ++
          methodName.getBytes ++
          (sender._2)._2.pubKeyBytes ++
          parameters.noSpaces.getBytes ++
          contractBox.id ++
          Longs.toByteArray(timestamp) ++
          Longs.toByteArray(fee)
      )

      val messageToSign = FastCryptographicHash(contractBox.value.asObject.get("storage").get.noSpaces.getBytes ++ hashNoNonces)
      val signature = PrivateKey25519Companion.sign((sender._2)._1, messageToSign)

      ContractMethodExecution(
        contractBox,
        methodName,
        parameters,
        Map(sender._1 -> sender._2._2),
        Map(sender._2._2 -> signature),
        Map(sender._2._2 -> (0 until numFeeBoxes).map { _ => preFeeBoxGen.sample.get}),
        Map(sender._2._2 -> positiveTinyIntGen.sample.get.toLong),
        timestamp
      )
    }

    forAll(postExpContractMethodExecutionGen) {
      cme: ContractMethodExecution =>
        val semanticValid = BifrostState.semanticValidity(cme)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }

}
