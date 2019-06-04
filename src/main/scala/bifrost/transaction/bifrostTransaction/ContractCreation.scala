package bifrost.transaction.bifrostTransaction

import bifrost.contract.ExecutionBuilder
import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.Nonce
import Role.Role
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.box.{BifrostBox, BoxUnlocker, CodeBox, ContractBox, PolyBox, StateBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ContractCreationCompanion
import bifrost.transaction.state.PrivateKey25519
import bifrost.transaction.serialization.AgreementCompanion
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  *
  * //@param agreement          the Agreement object containing the terms for the proposed contract
  * @param preInvestmentBoxes a list of box nonces corresponding to the PolyBoxes to be used to fund the investment
  * @param parties            a mapping specifying which public key should correspond with which role for this contract
  * @param signatures         a mapping specifying the signatures by each public key for this transaction
  * @param preFeeBoxes        a mapping specifying box nonces and amounts corresponding to the PolyBoxes to be used to
  *                           pay fees for each party contributing fees
  * @param fees               a mapping specifying the amount each party is contributing to the fees
  * @param timestamp          the timestamp of this transaction
  */
case class ContractCreation(agreement: ExecutionBuilder,
                            preInvestmentBoxes: IndexedSeq[(Nonce, Long)],
                            parties: Map[PublicKey25519Proposition, Role],
                            signatures: Map[PublicKey25519Proposition, Signature25519],
                            preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                            fees: Map[PublicKey25519Proposition, Long],
                            timestamp: Long,
                            data: String)
  extends ContractTransaction {

  override type M = ContractCreation

  lazy val proposition = MofNProposition(1, parties.map(_._1.pubKeyBytes).toSet)

  //val allInvestorsSorted = parties.filter(_._2 == Role.Investor).toSeq.sortBy(_._1.pubKeyBytes.toString)


  lazy val investmentBoxIds: IndexedSeq[Array[Byte]] =
    preInvestmentBoxes.map(n => {
//      println(parties.head._1)
//      println(s">>>>>>>>>>>>  ${n._1}")
//      println(s">>>>>>>>>>>> preInvestmentBoxes: ${PublicKeyNoncedBox.idFromBox(parties.head._1, n._1)}")
      PublicKeyNoncedBox.idFromBox(parties.head._1, n._1)})

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = investmentBoxIds
    .map(id =>
           new BoxUnlocker[PublicKey25519Proposition] {
             override val closedBoxId: Array[Byte] = id
             override val boxKey: Signature25519 = signatures(parties.head._1)
           }
    ) ++ feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    AgreementCompanion.toBytes(agreement) ++
      parties.toSeq.sortBy(_._2).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).foldLeft(Array[Byte]())(_ ++ _) ++
      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
      fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)))

  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractTransaction.nonceFromDigest(digest)

    val boxValue: Json = Map(
        "parties" -> parties.map(kv => Base58.encode(kv._1.pubKeyBytes) -> kv._2.toString).asJson,
        "agreement" -> agreement.json,
        "lastUpdated" -> timestamp.asJson
      ).asJson

    val investor = parties.head
//    println(s">>>>>>> find investor: ${investor.toString()}")
    val investorProp = investor._1
    val availableBoxes: Set[(Nonce, Long)] = (preFeeBoxes(investorProp) ++ preInvestmentBoxes).toSet
    val canSend = availableBoxes.map(_._2).sum
    val leftOver: Long = canSend - fees(investorProp)

    val investorNonce = ContractTransaction.nonceFromDigest(
      FastCryptographicHash("ContractCreation".getBytes
        ++ investorProp.pubKeyBytes
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )
    val stateNonce = ContractTransaction.nonceFromDigest(
      FastCryptographicHash("ContractCreation".getBytes
        ++ agreement.core.variables.foldLeft(Array[Byte]())((a,b) => a ++ b.getBytes())
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )
    val codeNonce = ContractTransaction.nonceFromDigest(
      FastCryptographicHash("ContractCreation".getBytes
        ++ agreement.core.code.foldLeft(Array[Byte]())((a,b) => a ++ b.getBytes())
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )

    val stateBox = StateBox(investorProp, stateNonce, agreement.core.variables, true)
    val codeBox = CodeBox(investorProp, codeNonce, agreement.core.code)

    val investorDeductedBoxes: PolyBox = PolyBox(investorProp, investorNonce, leftOver)
    val nonInvestorDeductedBoxes: IndexedSeq[PolyBox] = deductedFeeBoxes(hashNoNonces).filter(_.proposition != investorProp)

    IndexedSeq(ContractBox(proposition, nonce, boxValue), stateBox, codeBox) ++ nonInvestorDeductedBoxes :+ investorDeductedBoxes
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "preInvestmentBoxes" -> preInvestmentBoxes.map(_.asJson).asJson,
    "agreement" -> agreement.json
  )).asJson

  override lazy val serializer = ContractCreationCompanion

//  println("BifrostTransaction")
  //println(AgreementCompanion.toBytes(agreement).mkString(""))
  //println(parties.toSeq.sortBy(_._1.pubKeyBytes.toString).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes).mkString(""))
//  println(investmentBoxIds.foldLeft(Array[Byte]())(_ ++ _).mkString(""))
//  println(preInvestmentBoxes)
  //println(feeBoxIdKeyPairs.map(_._1).foldLeft(Array[Byte]())(_ ++ _).mkString(""))

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    AgreementCompanion.toBytes(agreement),
    parties.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes),
    unlockers.toArray.flatMap(_.closedBoxId),
    data.getBytes
    //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _)
  )

//  println()
//  println(s">>>>>>>>>>> messageToSign direct: " + messageToSign.mkString(""))
//  println()

  override def toString: String = s"ContractCreation(${json.noSpaces})"
}

object ContractCreation {

  def validate(tx: ContractCreation): Try[Unit] = Try {

    val outcome = ExecutionBuilder.validate(tx.agreement)
    require(outcome.isSuccess)


    require((tx.parties.size == tx.signatures.size) && tx.parties.size >= 2,
      "There aren't exactly 3 parties involved in signing")
    require(tx.parties.size >= 2, "There aren't exactly 3 roles") // Make sure there are exactly 3 unique roles
    require(tx.parties.forall { case (proposition, _) =>
      tx.signatures(proposition).isValid(proposition, tx.messageToSign)
    }, "Not all signatures were valid")

  }.flatMap(_ => ContractTransaction.commonValidation(tx))

  implicit val decodeContractCreation: Decoder[ContractCreation] = (c: HCursor) => for {
    agreement <- c.downField("agreement").as[ExecutionBuilder]
    preInvestmentBoxes <- c.downField("preInvestmentBoxes").as[IndexedSeq[(Nonce, Long)]]
    rawParties <- c.downField("parties").as[Map[String, String]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ContractTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ContractCreation(agreement,
      preInvestmentBoxes,
      commonArgs._1,
      commonArgs._2,
      commonArgs._3,
      commonArgs._4,
      timestamp,
      data)
  }

}