package bifrost.transaction.bifrostTransaction

import java.util.UUID

import bifrost.program.ExecutionBuilder
import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.Nonce
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{BifrostBox, CodeBox, ExecutionBox, PolyBox, StateBox}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ProgramCreationCompanion
import bifrost.transaction.serialization.ExecutionBuilderCompanion
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  *
  * @param executionBuilder   the ExecutionBuilder object containing the terms for the proposed program
  * @param readOnlyStateBoxes a list of StateBoxes to be used in evaluating the program, but never mutated
  *                           beyond the context of the evaluation
  * @param preInvestmentBoxes a list of box nonces corresponding to the PolyBoxes to be used to fund the investment
  * @param owner              the public key used to sign and create newboxes
  * @param signatures         a mapping specifying the signatures by each public key for this transaction
  * @param preFeeBoxes        a mapping specifying box nonces and amounts corresponding to the PolyBoxes to be used to
  *                           pay fees for each party contributing fees
  * @param fees               a mapping specifying the amount each party is contributing to the fees
  * @param timestamp          the timestamp of this transaction
  */
case class ProgramCreation(executionBuilder: ExecutionBuilder,
                           readOnlyStateBoxes: Seq[UUID],
                           preInvestmentBoxes: IndexedSeq[(Nonce, Long)],
                           owner: PublicKey25519Proposition,
                           signatures: Map[PublicKey25519Proposition, Signature25519],
                           preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                           fees: Map[PublicKey25519Proposition, Long],
                           timestamp: Long,
                           data: String)
  extends ProgramTransaction {

  override type M = ProgramCreation

//  lazy val proposition = MofNProposition(1, parties.map(_._1.pubKeyBytes).toSet)

  lazy val investmentBoxIds: IndexedSeq[Array[Byte]] =
    preInvestmentBoxes.map(n => {
      PublicKeyNoncedBox.idFromBox(owner, n._1)})

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)

  lazy val hashNoNonces = FastCryptographicHash(
    ExecutionBuilderCompanion.toBytes(executionBuilder) ++
      owner.pubKeyBytes ++
      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
      fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)))

  lazy val newStateBoxes: Traversable[StateBox] = {
      val stateNonce = ProgramTransaction.nonceFromDigest(
        FastCryptographicHash("stateBox".getBytes
          ++ executionBuilder.core.variables.noSpaces.getBytes
          ++ hashNoNonces
          ++ Ints.toByteArray(0))
      )

    val stateBox = StateBox(owner, stateNonce, UUID.nameUUIDFromBytes(StateBox.idFromBox(owner, stateNonce)), executionBuilder.core.variables)

    IndexedSeq(stateBox)
  }

  override lazy val newBoxes: Traversable[BifrostBox] = {

    val digest = FastCryptographicHash(owner.pubKeyBytes ++ hashNoNonces)

    val nonce = ProgramTransaction.nonceFromDigest(digest)

    val boxValue: Json = Map(
        "owner" -> Base58.encode(owner.pubKeyBytes).asJson,
        "executionBuilder" -> executionBuilder.json,
        "lastUpdated" -> timestamp.asJson
      ).asJson

    val availableBoxes: Set[(Nonce, Long)] = (preFeeBoxes(owner) ++ preInvestmentBoxes).toSet
    val canSend = availableBoxes.map(_._2).sum
    val leftOver: Long = canSend - fees(owner)

    val investorNonce = ProgramTransaction.nonceFromDigest(
      FastCryptographicHash("ProgramCreation".getBytes
        ++ owner.pubKeyBytes
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )

    val codeNonce = ProgramTransaction.nonceFromDigest(
      FastCryptographicHash("codeBox".getBytes
        ++ executionBuilder.core.code.values.foldLeft(Array[Byte]())((a,b) => a ++ b.getBytes())
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )
    val execNonce = ProgramTransaction.nonceFromDigest(
      FastCryptographicHash("executionBuilder".getBytes
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )


    val stateNonce = ProgramTransaction.nonceFromDigest(
      FastCryptographicHash("stateBox".getBytes
        ++ executionBuilder.core.variables.noSpaces.getBytes
        ++ hashNoNonces
        ++ Ints.toByteArray(0))
    )

    val stateBox = StateBox(owner, stateNonce, UUID.nameUUIDFromBytes(StateBox.idFromBox(owner, stateNonce)),executionBuilder.core.variables)

    val codeBox = CodeBox(owner, codeNonce, UUID.nameUUIDFromBytes(CodeBox.idFromBox(owner, codeNonce)),
      executionBuilder.core.code.values.toSeq, executionBuilder.core.interface)
    val stateUUIDs: Seq[UUID] = Seq(UUID.nameUUIDFromBytes(stateBox.id)) ++ readOnlyStateBoxes
    val executionBox = ExecutionBox(owner, execNonce, UUID.nameUUIDFromBytes(ExecutionBox.idFromBox(owner, execNonce)), stateUUIDs, Seq(codeBox.id))

    val investorDeductedBoxes: PolyBox = PolyBox(owner, investorNonce, leftOver)

    IndexedSeq(executionBox, stateBox, codeBox) :+ investorDeductedBoxes // nonInvestorDeductedBoxes
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "preInvestmentBoxes" -> preInvestmentBoxes.map(_.asJson).asJson,
    "executionBuilder" -> executionBuilder.json,
    "newBoxes" -> newBoxes.map(_.json).asJson,
    "data" -> data.asJson
  )).asJson

  override lazy val serializer = ProgramCreationCompanion

//  println("BifrostTransaction")
  //println(ExecutionBuilderCompanion.toBytes(executionBuilder).mkString(""))
  //println(parties.toSeq.sortBy(_._1.pubKeyBytes.toString).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes).mkString(""))
//  println(investmentBoxIds.foldLeft(Array[Byte]())(_ ++ _).mkString(""))
//  println(preInvestmentBoxes)
  //println(feeBoxIdKeyPairs.map(_._1).foldLeft(Array[Byte]())(_ ++ _).mkString(""))

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    ExecutionBuilderCompanion.toBytes(executionBuilder),
    owner.pubKeyBytes,
    data.getBytes
    //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _)
  )

  override def toString: String = s"ProgramCreation(${json.noSpaces})"
}

object ProgramCreation {

  def validate(tx: ProgramCreation): Try[Unit] = Try {

    val outcome = ExecutionBuilder.validate(tx.executionBuilder)
    require(outcome.isSuccess)

    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign), "Not all signatures were valid")

  }.flatMap(_ => ProgramTransaction.commonValidation(tx))

  implicit val decodeProgramCreation: Decoder[ProgramCreation] = (c: HCursor) => for {
    executionBuilder <- c.downField("executionBuilder").as[ExecutionBuilder]
    readOnlyStateBoxes <- c.downField("readOnlyStateBoxes").as[Seq[UUID]]
    preInvestmentBoxes <- c.downField("preInvestmentBoxes").as[IndexedSeq[(Nonce, Long)]]
    rawOwner <- c.downField("owner").as[String]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ProgramTransaction.commonDecode(rawOwner, rawSignatures, rawPreFeeBoxes, rawFees)
    ProgramCreation(executionBuilder,
      readOnlyStateBoxes,
      preInvestmentBoxes,
      commonArgs._1,
      commonArgs._2,
      commonArgs._3,
      commonArgs._4,
      timestamp,
      data)
  }

}