package examples.bifrost.transaction

import examples.bifrost.transaction.contract._
import io.circe.Json
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.Transaction
import scorex.crypto.encode.Base58

sealed trait BifrostTransaction extends Transaction[PublicKey25519Proposition]


/** CONTRACT TRANSACTIONS **/

sealed abstract class ContractTransaction extends BifrostTransaction

case class ContractCreation(sender: PublicKey25519Proposition,
                            agreement: Agreement,
                            fee: Long,
                            nonce: Long,
                            timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCreation

  override lazy val json: Json = Map(
    "sender" -> Base58.encode(sender.pubKeyBytes).asJson,
    "agreement" -> agreement.json,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCreationCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"ContractCreation(${json.noSpaces})"

}


case class Agreement(senders: (PublicKey25519Proposition, PublicKey25519Proposition, PublicKey25519Proposition),
                     terms: AgreementTerms,
                     nonce: Long,
                     fee: Long = 0,
                     timestamp: Long)
  extends ContractTransaction {

  override type M = Agreement

  override lazy val json: Json = Map(
    "senders" -> Array(
      Base58.encode(senders._1.pubKeyBytes),
      Base58.encode(senders._2.pubKeyBytes),
      Base58.encode(senders._3.pubKeyBytes)).asJson,
    "terms" -> terms.json,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = AgreementCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"Agreement(${json.noSpaces})"

}


trait PaymentTransaction extends BifrostTransaction

case class BifrostPayment(sender: PublicKey25519Proposition,
                         recipient: PublicKey25519Proposition,
                         amount: Long,
                         fee: Long,
                         nonce: Long,
                         timestamp: Long)
  extends PaymentTransaction {

  override type M = BifrostPayment

  override lazy val json: Json = Map(
    "sender" -> Base58.encode(sender.pubKeyBytes).asJson,
    "recipient" -> Base58.encode(recipient.pubKeyBytes).asJson,
    "amount" -> amount.asJson,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val messageToSign: Array[Byte] = id

  override lazy val serializer = BifrostPaymentCompanion

  override def toString: String = s"BifrostPayment(${json.noSpaces})"
}