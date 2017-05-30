package bifrost

import bifrost.contract.{Agreement, Contract}
import bifrost.transaction._
import bifrost.transaction.box.proposition.MofNProposition
import bifrost.transaction.box.{ContractBox, ProfileBox}
import com.google.common.primitives.{Bytes, Longs}
import org.scalacheck.Gen
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.crypto.encode.Base58
import io.circe.syntax._
import scorex.core.transaction.state.PrivateKey25519Companion

import scala.util.Random

/**
  * Created by cykoz on 5/11/2017.
  */
trait ValidGenerators extends BifrostGenerators {

  lazy val validAgreementGen: Gen[Agreement] = for {
    terms <- agreementTermsGen
    timestamp <- positiveLongGen
  } yield Agreement(terms, timestamp)

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
      Longs.toByteArray(timestamp),
      AgreementCompanion.toBytes(agreement),
      parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes)
    )
    val signatures = allKeyPairs.map(
      keypair =>
        PrivateKey25519Companion.sign(keypair._1, messageToSign)
    )
    ContractCreation(agreement, IndexedSeq(Role.Investor, Role.Producer, Role.Hub).zip(parties), signatures, fee, timestamp)
  }

  lazy val contractMethodExecutionGen: Gen[ContractMethodExecution] = for {
    contract <- contractBoxGen
    methodName <- stringGen
    parameters <- jsonArrayGen()
    sigSeq <- sigSeqGen
    fee <- positiveLongGen
    timestamp <- positiveLongGen
    party <- propositionGen
  } yield ContractMethodExecution(contract, Gen.oneOf(Role.values.toSeq).sample.get -> party, methodName, parameters, sigSeq, fee, timestamp)

  lazy val validContractCompletionGen: Gen[ContractCompletion] = for {
    fee <- positiveLongGen
    timestamp <- positiveLongGen
  } yield {
    val allKeyPairs = (0 until 3).map(_ => keyPairSetGen.sample.get.head)
    val parties = allKeyPairs.map(_._2)

    val contract = Contract(Map(
      "producer" -> Base58.encode(parties(0).pubKeyBytes).asJson,
      "investor" -> Base58.encode(parties(1).pubKeyBytes).asJson,
      "hub" -> Base58.encode(parties(2).pubKeyBytes).asJson,
      "storage" -> Map(
        "status" -> Gen.oneOf(validStatuses).sample.get.asJson,
        "currentFulfillment" -> Map(
          "deliveredQuantity" -> positiveLongGen.sample.get.asJson
        ).asJson,
        "other" -> jsonGen().sample.get
      ).asJson,
      "agreement" -> validAgreementGen.sample.get.json
    ).asJson, genBytesList(FastCryptographicHash.DigestSize).sample.get)

    val proposition = MofNProposition(1, parties.map(_.pubKeyBytes).toSet)
    val contractBox = ContractBox(proposition, positiveLongGen.sample.get, contract.json)

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

  lazy val validContractMethods: List[String] = List("endorseCompletion", "currentStatus", "deliver", "confirmDelivery", "checkExpiration")

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
