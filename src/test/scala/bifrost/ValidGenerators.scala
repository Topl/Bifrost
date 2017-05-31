package bifrost

import java.time.Instant

import bifrost.contract.{Agreement, Contract}
import bifrost.transaction._
import bifrost.transaction.box.proposition.MofNProposition
import bifrost.transaction.box.{ContractBox, ProfileBox}
import com.google.common.primitives.{Bytes, Longs}
import io.circe.Json
import org.scalacheck.Gen
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.crypto.encode.Base58
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion

import scala.util.Random

/**
  * Created by cykoz on 5/11/2017.
  */
trait ValidGenerators extends BifrostGenerators {

  lazy val validAgreementGen: Gen[Agreement] = for {
    terms <- agreementTermsGen
    delta <- positiveLongGen
  } yield Agreement(terms, Instant.now.toEpochMilli + 10000L, Instant.now.toEpochMilli + 10000L + delta)

  // TODO this should be an enum
  val validStatuses: List[String] = List("expired", "complete", "initialized")

  lazy val validContractGen: Gen[Contract] = for {
    producer <- propositionGen
    investor <- propositionGen
    hub <- propositionGen
    storage <- jsonGen()
    status <- Gen.oneOf(validStatuses)
    agreement <- validAgreementGen.map(_.json)
    id <- genBytesList(FastCryptographicHash.DigestSize)
  } yield Contract(Map(
    "producer" -> Base58.encode(producer.pubKeyBytes).asJson,
    "investor" -> Base58.encode(investor.pubKeyBytes).asJson,
    "hub" -> Base58.encode(hub.pubKeyBytes).asJson,
    "storage" -> Map("status" -> status.asJson, "other" -> storage).asJson,
    "agreement" -> agreement
  ).asJson, id)

  lazy val validContractCreationGen: Gen[ContractCreation] = for {
    agreement <- validAgreementGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)
    val messageToSign = Bytes.concat(
      AgreementCompanion.toBytes(agreement),
      parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes)
    )
    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )
    ContractCreation(agreement, IndexedSeq(Role.Investor, Role.Producer, Role.Hub).zip(parties), signatures, fee, timestamp)
  }

  lazy val validContractMethods: List[String] = List("endorseCompletion", "currentStatus", "deliver", "confirmDelivery", "checkExpiration")

  def createContractBox(agreement: Agreement,
                        status: String,
                        currentFulfillment: Map[String, Json],
                        parties: IndexedSeq[PublicKey25519Proposition]): ContractBox = {

    val contract = Contract(Map(
      "producer" -> Base58.encode(parties(0).pubKeyBytes).asJson,
      "investor" -> Base58.encode(parties(1).pubKeyBytes).asJson,
      "hub" -> Base58.encode(parties(2).pubKeyBytes).asJson,
      "storage" -> Map(
        "status" -> status.asJson,
        "currentFulfillment" -> currentFulfillment.asJson,
        "other" -> jsonGen().sample.get
      ).asJson,
      "agreement" -> agreement.json
    ).asJson, genBytesList(FastCryptographicHash.DigestSize).sample.get)

    val proposition = MofNProposition(1, parties.map(_.pubKeyBytes).toSet)
    ContractBox(proposition, positiveLongGen.sample.get, contract.json)
  }

  lazy val semanticallyValidContractMethodExecutionGen: Gen[ContractMethodExecution] = for {
    methodName <- Gen.oneOf(validContractMethods)
    parameters <- jsonArrayGen()
    fee <- positiveLongGen
    timestamp <- positiveLongGen.map(_ / 3)
    status <- Gen.oneOf(validStatuses)
    deliveredQuantity <- positiveLongGen
    effDelta <- positiveLongGen.map(_ / 3)
    expDelta <- positiveLongGen.map(_ / 3)
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)

    val currentFulfillment = Map("deliveredQuantity" -> deliveredQuantity.asJson)

    val contractBox = createContractBox(
      Agreement(agreementTermsGen.sample.get, timestamp - effDelta, timestamp + expDelta),
      "initialized",
      currentFulfillment,
      parties
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

    ContractMethodExecution(contractBox, sender._1 -> (sender._2)._2, methodName, parameters, signature, fee, timestamp)
  }


  lazy val validContractCompletionGen: Gen[ContractCompletion] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    agreement <- validAgreementGen
    status <- Gen.oneOf(validStatuses)
    deliveredQuantity <- positiveLongGen
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)

    val currentFulfillment = Map("deliveredQuantity" -> deliveredQuantity.asJson)

    val contractBox = createContractBox(agreement, status, currentFulfillment, parties)

    val messageToSign = FastCryptographicHash(
      contractBox.id ++
        parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes) ++
        contractBox.id ++
        Longs.toByteArray(timestamp) ++
        Longs.toByteArray(fee)
    )

    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )

    ContractCompletion(contractBox, IndexedSeq(), IndexedSeq(Role.Producer, Role.Investor, Role.Hub).zip(parties), signatures, fee, timestamp)
  }

  lazy val validPolyTransferGen: Gen[PolyTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = IndexedSeq((fromKeyPairs._1, Longs.fromByteArray(FastCryptographicHash("Testing").take(8))))
    val toKeyPairs = keyPairSetGen.sample.get.head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    PolyTransfer(from, to, fee, timestamp)
  }

  lazy val validArbitTransferGen: Gen[ArbitTransfer] = for {
    from <- fromSeqGen
    to <- toSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = IndexedSeq((fromKeyPairs._1, Longs.fromByteArray(FastCryptographicHash("Testing").take(8))))
    val toKeyPairs = keyPairSetGen.sample.get.head
    val to = IndexedSeq((toKeyPairs._2, 4L))

    ArbitTransfer(from, to, fee, timestamp)
  }
  lazy val validProfileTransactionGen: Gen[ProfileTransaction] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val rnd = new Random
    val keyValues = Map(
      "role" -> ProfileBox.acceptableRoleValues.toVector(rnd.nextInt(ProfileBox.acceptableRoleValues.size))
    )
    val fromKeyPairs = keyPairSetGen.sample.get.head
    val from = fromKeyPairs._2
    val signature = PrivateKey25519Companion.sign(fromKeyPairs._1, ProfileTransaction.messageToSign(timestamp, from, keyValues))
    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }

}
