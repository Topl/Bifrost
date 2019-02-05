package bifrost.transaction.bifrostTransaction

import bifrost.contract.Contract
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.exceptions.TransactionValidationException
import BifrostTransaction.Nonce
import Role.Role
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.{MultiSignature25519, Proof, Signature25519}
import bifrost.transaction.state.PrivateKey25519
import bifrost.transaction.serialization.ContractCompletionCompanion
import com.google.common.primitives.{Bytes, Longs}
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

case class ContractCompletion(contractBox: ContractBox,
                              producerReputation: IndexedSeq[ReputationBox],
                              parties: Map[PublicKey25519Proposition, Role],
                              signatures: Map[PublicKey25519Proposition, Signature25519],
                              preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                              fees: Map[PublicKey25519Proposition, Long],
                              timestamp: Long,
                              data: String)
  extends ContractTransaction {

  import ContractCompletion._

  override lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = Option(
    IndexedSeq("ContractCompletion".getBytes) ++ parties.map(_._1.pubKeyBytes)
  )

  override type M = ContractCompletion

  lazy val contract = Contract(contractBox.json.asObject.get.apply("value").get, contractBox.id)

  lazy val proposition = MofNProposition(1, contract.parties.map(_._1.pubKeyBytes).toSet)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq(contractBox.id) ++ feeBoxIdKeyPairs.map(_._1) //++ producerReputation.map(_.id)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Seq(
    new BoxUnlocker[MofNProposition] {
      override val closedBoxId: Array[Byte] = contractBox.id
      override val boxKey: Proof[MofNProposition] = MultiSignature25519(signatures.values.toSet)
    }
  ) ++
//    boxIdsToOpen.tail.take(producerReputation.length).map(id =>
//      new BoxUnlocker[PublicKey25519Proposition] {
//        override val closedBoxId: Array[Byte] = id
//        override val boxKey: Signature25519 = signatures(parties.find(_._2 == Role.Producer).get._1)
//      }
//    ) ++
    feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    contractBox.id ++
      //producerReputation.foldLeft(Array[Byte]())((concat, box) => concat ++ box.id) ++
      parties.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
      Longs.toByteArray(contract.lastUpdated) ++
      fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractTransaction.nonceFromDigest(digest)

    val assetCode: String = contract.getFromContract("assetCode").get.noSpaces
    val issuer: PublicKey25519Proposition = contract.parties.find(_._2 == "issuer").get._1

    val partyAssets: IndexedSeq[AssetBox] = contract.parties.map { p =>
      AssetBox(p._1, assetNonce(p._1, hashNoNonces), 0, assetCode, issuer, data)
    }.toIndexedSeq

    IndexedSeq(
      ReputationBox(parties.find(_._2 == Role.Producer).get._1, nonce, (0, 0))
    ) ++
      partyAssets ++
      deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "contractBox" -> contractBox.json
  )).asJson

  override lazy val serializer = ContractCompletionCompanion

  override lazy val messageToSign: Array[Byte] = Bytes.concat(hashNoNonces, data.getBytes)

  override def toString: String = s"ContractCompletion(${json.noSpaces})"

}

object ContractCompletion {

  def validate(tx: ContractCompletion): Try[Unit] = Try {
    if (tx.signatures.size != tx.parties.size) {
      throw new Exception("Inappropriate number of parties signed the completion")
    }

    if (!tx.parties.forall { case (proposition, _) =>
      val sig = Set(tx.signatures(proposition))
      val multiSig = MultiSignature25519(sig)
      val first = tx.signatures(proposition).isValid(proposition, tx.messageToSign)
      val second = multiSig.isValid(tx.contractBox.proposition, tx.messageToSign)

      first && second
    }) {
      throw new TransactionValidationException("Not all party signatures were valid")
    }

  }.flatMap(_ => ContractTransaction.commonValidation(tx))

  implicit val decodeContractCompletion: Decoder[ContractCompletion] = (c: HCursor) => for {
    contractBox <- c.downField("contractBox").as[ContractBox]
    reputationBoxes <- c.downField("reputationBoxes").as[IndexedSeq[ReputationBox]]
    rawParties <- c.downField("parties").as[Map[String, String]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ContractTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ContractCompletion(contractBox, reputationBoxes, commonArgs._1, commonArgs._2, commonArgs._3, commonArgs._4, timestamp, data)
  }

  def assetNonce(prop: PublicKey25519Proposition, hashNoNonces: Array[Byte]): Nonce = ContractTransaction
    .nonceFromDigest(
      FastCryptographicHash("ContractCompletion".getBytes
        ++ prop.pubKeyBytes
        ++ hashNoNonces)
    )

}